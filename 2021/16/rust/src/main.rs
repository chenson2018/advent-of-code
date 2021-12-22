use std::cell::Cell;
use std::fmt;

fn create(trans: &str) -> Vec<char> {
  let chars: Vec<char> = trans.chars().collect();
  let bin: Vec<String> = chars.iter().map(|c| format!("{:04b}", c.to_digit(16).unwrap() ).to_string() ).collect();

  let mut res = Vec::new();
  
  for byte in bin {
    res.extend(byte.chars())
  }
  res
}

#[derive(Debug)]
struct Literal {
  head: Header,
  value: u64  
}

#[derive(Debug)]
struct Header {
  version: u8,
  packet_type: u8,
}

#[derive(Debug)]
enum Packet {
  Value(Literal),
  SubPacket(Operator)
}

#[derive(Debug)]
struct Operator {
  head: Header,
  len_type: u32,
  len: u16,
  packets: Vec<Packet>
}


impl From<&Parser<'_>> for Header {
  fn from(parser: &Parser) -> Self {
    // store the current index so we can increment now 
    let idx = parser.index.get();
    parser.inc(6);

    let version =  u8::from_str_radix(&parser.bits[idx  ..=idx+2].iter().collect::<String>(), 2).unwrap();
    let packet_type = u8::from_str_radix(&parser.bits[idx+3..=idx+5].iter().collect::<String>(), 2).unwrap();

    Header { version, packet_type }
  }
}

struct Parser<'a> {
  bits: &'a Vec<char>,
  index: Cell<usize>
}

impl Parser<'_> {
  fn inc(&self, dist: usize) {
    self.index.set(self.index.get() + dist)
  }

  fn get_literal(&self) -> u64 {
    let mut res = Vec::new();
    let init_idx = self.index.get();

    loop {
      let continue_bit = self.bits[self.index.get()];
      res.extend_from_slice(&self.bits[self.index.get()+1..self.index.get()+5]);
      self.inc(5);
      if continue_bit == '0' {
        break;
      }
    }

    //println!("Started at {}, end at {}, push up {}", init_idx, self.index.get(), (self.index.get()-init_idx-1).rem_euclid(4));
    self.inc((self.index.get()-init_idx-1).rem_euclid(4));
    u64::from_str_radix(&res.iter().collect::<String>(), 2).unwrap()
  }

  fn get_len_type(&self) -> u32 {
    let idx = self.index.get();
    self.inc(1);
    self.bits[idx].to_digit(10).unwrap()
  }

  fn get_len(&self, len_type: u32) -> u16 {
    let idx = self.index.get();
    if len_type == 0 {
      self.inc(15);
      u16::from_str_radix(&self.bits[idx..=idx+14].iter().collect::<String>(), 2).unwrap()
    } else if len_type == 1 {
      self.inc(11);
      u16::from_str_radix(&self.bits[idx..=idx+10].iter().collect::<String>(), 2).unwrap()
    } else {
      panic!("Len type {} not implemented", len_type)
    }
  }

  fn get_next(&self) -> Packet {
    let head = Header::from(self);
    if head.packet_type == 4 {
      let value = self.get_literal();
      Packet::Value(Literal { head, value })
    } else {
      let len_type = self.get_len_type();
      let len = self.get_len(len_type);

      let init_idx = self.index.get();

      let mut packets = Vec::new();

      // should make these matches!

      if len_type == 0 {
        // edge case potential...
        while self.index.get() - init_idx < (len as usize) {
          packets.push(self.get_next());
        }
      } else if len_type == 1 {
        for _ in 0..(len as usize) {
          packets.push(self.get_next());
        }
      } else {
        panic!("Len type {} while loop not implemented", len_type)
      }

      // no idea if this is right...
      loop {
        match self.bits.get(self.index.get()) {
          Some('0') => { self.inc(1); },
          Some(_)   => {  break; },
          None      => {  break; },
        }
      }

      Packet::SubPacket(Operator { head, len_type, len, packets })
    } 
  }
}

impl fmt::Display for Parser<'_> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{} -> {}", &self.bits[0               ..self.index.get()].iter().collect::<String>(),
                          &self.bits[self.index.get()..                ].iter().collect::<String>())
  }
}

fn main() {
  //let input  = "D2FE28";
  //let input  = "38006F45291200";
  //let input  = "EE00D40C823060";
  let input  = "8A004A801A8002F478";
//  let input  = "620080001611562C8802118E34";
//  let input  = "620D7800996600E43184312CC01A88913E1E180310FA324649CD5B9DA6BFD107003A4FDE9C718593003A5978C00A7003C400A70025400D60259D400B3002880792201B89400E601694804F1201119400C600C144008100340013440021279A5801AE93CA84C10CF3D100875401374F67F6119CA46769D8664E76FC9E4C01597748704011E4D54D7C0179B0A96431003A48ECC015C0068670FA7EF1BC5166CE440239EFC226F228129E8C1D6633596716E7D4840129C4C8CA8017FCFB943699B794210CAC23A612012EB40151006E2D4678A4200EC548CF12E4FDE9BD4A5227C600F80021D08219C1A00043A27C558AA200F4788C91A1002C893AB24F722C129BDF5121FA8011335868F1802AE82537709999796A7176254A72F8E9B9005BD600A4FD372109FA6E42D1725EDDFB64FFBD5B8D1802323DC7E0D1600B4BCDF6649252B0974AE48D4C0159392DE0034B356D626A130E44015BD80213183A93F609A7628537EB87980292A0D800F94B66546896CCA8D440109F80233ABB3ABF3CB84026B5802C00084C168291080010C87B16227CB6E454401946802735CA144BA74CFF71ADDC080282C00546722A1391549318201233003361006A1E419866200DC758330525A0C86009CC6E7F2BA00A4E7EF7AD6E873F7BD6B741300578021B94309ABE374CF7AE7327220154C3C4BD395C7E3EB756A72AC10665C08C010D0046458E72C9B372EAB280372DFE1BCA3ECC1690046513E5D5E79C235498B9002BD132451A5C78401B99AFDFE7C9A770D8A0094EDAC65031C0178AB3D8EEF8E729F2C200D26579BEDF277400A9C8FE43D3030E010C6C9A078853A431C0C0169A5CB00400010F8C9052098002191022143D30047C011100763DC71824200D4368391CA651CC0219C51974892338D0";
  let bits   = create(input);
  let parser = Parser { bits: &bits, index: Cell::new(0) };

  let next = parser.get_next();
  println!("{:?}", next);
  println!("Idx: {:?}", parser.index);
  println!("Display: {}", parser);
}
