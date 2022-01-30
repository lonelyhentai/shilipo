use nom::AsChar;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CSSToken {
  Comment { text: String },
  Ident { text: String },
  Function { text: String },
  AtKeyword { text: String },
  Hash { text: String },
  String { text: String },
  Url { text: String },
  Number { number: String },
  Dimension { number: String, text: String },
  Percentage { number: String },
  Delim { text: String },
  Semicolon,
  Comma,
  Cdo,
  Cdc,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CSSComponentValue {
  FunctionBlock(CSSFunctionBlock),
  CurlyBracesBlock(CSSCurlyBracesBlock),
  RoundBracesBlock(CSSRoundBracesBlock),
  SquareBracesBlock(CSSSquareBracesBlock),
  PreservedToken(CSSToken),
}

impl CSSComponentValue {
  pub fn get_ident(&self) -> Option<&str> {
    match self {
      CSSComponentValue::PreservedToken(CSSToken::Ident { text: ref ident }) => Some(ident as &str),
      _ => None,
    }
  }

  pub fn get_dimension(&self) -> Option<(f32, &str)> {
    match self {
      CSSComponentValue::PreservedToken(CSSToken::Dimension {
        text: ref unit,
        number: ref num,
      }) => num.parse::<f32>().ok().map(|n| (n, unit as &str)),
      _ => None,
    }
  }

  pub fn get_number(&self) -> Option<f32> {
    match self {
      CSSComponentValue::PreservedToken(CSSToken::Number { number: ref num }) => {
        num.parse::<f32>().ok()
      }
      _ => None,
    }
  }

  pub fn to_length(&self) -> Option<CSSLength> {
    match self {
      CSSComponentValue::PreservedToken(CSSToken::Ident { text: ref ident }) => match ident as &str
      {
        "auto" => Some(CSSLength::Auto),
        _ => None,
      },
      CSSComponentValue::PreservedToken(CSSToken::Dimension {
        text: ref unit,
        number: ref num,
      }) => num.parse::<f32>().ok().and_then(|n| match unit as &str {
        "px" => Some(CSSLength::Dimension {
          number: n,
          unit: CSSLengthUnit::Px,
        }),
        _ => None,
      }),
      CSSComponentValue::PreservedToken(CSSToken::Number { number: ref num }) => num
        .parse::<f32>()
        .ok()
        .map(|n| CSSLength::Number { number: n }),
      _ => None,
    }
  }

  pub fn to_color(&self) -> Option<CSSColor> {
    match self {
      CSSComponentValue::PreservedToken(CSSToken::Hash { text: ref color }) => {
        if color.len() == 6 && color.chars().any(|c: char| c.is_hex_digit()) {
          Some(CSSColor::Hex {
            color: color.into(),
          })
        } else {
          None
        }
      }
      _ => None,
    }
  }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CSSLengthUnit {
  Px,
}

#[derive(Debug, Clone)]
pub enum CSSLength {
  Number { number: f32 },
  Dimension { number: f32, unit: CSSLengthUnit },
  Auto,
}

impl CSSLength {
  pub fn is_auto(&self) -> bool {
    matches!(self, &CSSLength::Auto)
  }

  pub fn to_px(&self) -> f32 {
    match self {
      CSSLength::Number { number } => *number,
      CSSLength::Dimension { number, unit } => match unit {
        CSSLengthUnit::Px => *number,
        #[allow(unreachable_patterns)]
        _ => 0f32,
      },
      CSSLength::Auto => 0f32,
    }
  }
}

impl PartialEq for CSSLength {
  fn eq(&self, other: &Self) -> bool {
    (self.is_auto() && other.is_auto())
      || (!self.is_auto() && !self.is_auto() && self.to_px() == other.to_px())
  }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CSSColor {
  Hex { color: String },
  Transparent,
}

impl CSSColor {
  pub fn to_rgba8(&self) -> (u8, u8, u8, u8) {
    match self {
      CSSColor::Hex { color } => (
        u8::from_str_radix(&color[0..=1], 16).unwrap_or(0u8),
        u8::from_str_radix(&color[2..=3], 16).unwrap_or(0u8),
        u8::from_str_radix(&color[4..=5], 16).unwrap_or(0u8),
        255u8,
      ),
      CSSColor::Transparent => (255u8, 255u8, 255u8, 0u8),
    }
  }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CSSFunctionBlock {
  pub function_name: String,
  pub components: Vec<CSSComponentValue>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CSSCurlyBracesBlock {
  pub components: Vec<CSSComponentValue>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CSSRoundBracesBlock {
  pub components: Vec<CSSComponentValue>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CSSSquareBracesBlock {
  pub components: Vec<CSSComponentValue>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CSSImportant {
  pub is_important: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CSSDeclaration {
  pub ident: String,
  pub components: Vec<CSSComponentValue>,
  pub important: CSSImportant,
}

impl CSSDeclaration {
  pub fn nth_component(&self, idx: usize) -> Option<&CSSComponentValue> {
    self.components.get(idx)
  }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CSSDisplay {
  Inline,
  Replace,
  Block,
  None,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CSSSelectorItem {
  Class { text: String },
  Id { text: String },
  Tag { text: String },
  All,
  Whitespaces,
  Unknown { text: String },
}

impl CSSSelectorItem {
  pub fn is_simple(&self) -> bool {
    matches!(
      self,
      Self::Class { .. } | Self::Id { .. } | Self::Tag { .. } | Self::All
    )
  }
}

pub type CSSSpecificity = (u32, u32, u32, u32);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CSSSelector {
  pub items: Vec<CSSSelectorItem>,
}

impl CSSSelector {
  pub fn specificity(&self) -> Option<CSSSpecificity> {
    if self.can_specificity() {
      let inline = 0u32;
      let mut id = 0u32;
      let mut class = 0u32;
      let mut tag = 0u32;
      for i in &self.items {
        match i {
          CSSSelectorItem::Id { .. } => id += 1,
          CSSSelectorItem::Tag { .. } => tag += 1,
          CSSSelectorItem::Class { .. } => class += 1,
          _ => {}
        };
      }
      Some((inline, id, class, tag))
    } else {
      None
    }
  }

  pub fn is_simple(&self) -> bool {
    self.items.iter().all(|f| f.is_simple())
  }

  pub fn can_specificity(&self) -> bool {
    self.is_simple()
  }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CSSSelectorList {
  pub items: Vec<CSSSelector>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CSSQualifiedRuleSelectors {
  pub selectors: CSSSelectorList,
  pub declaration_list: CSSDeclarationList,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CSSQualifiedRuleUnknown {
  pub components: Vec<CSSComponentValue>,
  pub block: CSSCurlyBracesBlock,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CSSQualifiedRule {
  Selectors(CSSQualifiedRuleSelectors),
  Unknown(CSSQualifiedRuleUnknown),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CSSAtRuleFontFace {
  pub declaration_list: CSSDeclarationList,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CSSAtRuleKeyframes {
  pub keyframes_name: String,
  pub keyframes: CSSKeyframeRuleList,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CSSAtRuleUnknown {
  pub at_keyword: String,
  pub components: Vec<CSSComponentValue>,
  pub block: Option<CSSCurlyBracesBlock>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CSSAtRule {
  FontFace(CSSAtRuleFontFace),
  Keyframes(CSSAtRuleKeyframes),
  Unknown(CSSAtRuleUnknown),
  /* not implement
  // Import
  // Media,
  // FontFeatureValues,
  // CountStyle
  // Document
  // Page
  // Namespace
  // Viewport
   **/
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CSSRuleList {
  pub items: Vec<CSSRuleListItem>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CSSRuleListItem {
  AtRule(CSSAtRule),
  QualifiedRule(CSSQualifiedRule),
  Whitespaces,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CSSKeyframeRule {
  pub selectors: Vec<CSSToken>,
  pub declaration_list: CSSDeclarationList,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CSSKeyframeRuleList {
  pub items: Vec<CSSKeyframeRule>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CSSDeclarationList {
  pub items: Vec<CSSDeclarationListItem>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CSSDeclarationListItem {
  AtRule(CSSAtRule),
  Declaration(CSSDeclaration),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CSSStylesheet {
  pub items: Vec<CSSStylesheetItem>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CSSStylesheetItem {
  Whitespaces,
  QualifiedRule(CSSQualifiedRule),
  AtRule(CSSAtRule),
  Cdo,
  Cdc,
}
