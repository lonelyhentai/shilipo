use crate::css::cssom::{CSSColor, CSSComponentValue};
use crate::layout::layout_box::{LayoutBox, Rect};
use image::{ImageBuffer, Rgba};

#[derive(Debug, PartialEq, Clone)]
pub struct Canvas {
  pub pixels: Vec<CSSColor>,
  pub width: usize,
  pub height: usize,
}

impl Canvas {
  pub fn new(width: usize, height: usize) -> Canvas {
    let white = CSSColor::Transparent;
    Canvas {
      pixels: vec![white; width * height],
      width,
      height,
    }
  }

  pub fn paint_item(&mut self, item: &PaintCommand) {
    match item {
      PaintCommand::FillColor(color, rect) => {
        // Clip the rectangle to the canvas boundaries.
        let x0 = rect.x.clamp(0.0, self.width as f32) as usize;
        let y0 = rect.y.clamp(0.0, self.height as f32) as usize;
        let x1 = (rect.x + rect.width).clamp(0.0, self.width as f32) as usize;
        let y1 = (rect.y + rect.height).clamp(0.0, self.height as f32) as usize;

        for y in y0..y1 {
          for x in x0..x1 {
            self.pixels[y * self.width + x] = color.clone();
          }
        }
      }
    }
  }

  pub fn to_image_buf(&self) -> ImageBuffer<Rgba<u8>, Vec<u8>> {
    let imgx = self.width as u32;
    let imgy = self.height as u32;

    let mut imgbuf = ImageBuffer::new(imgx, imgy);

    for (x, y, pixel) in imgbuf.enumerate_pixels_mut() {
      let i = y * imgx + x;
      let (r, g, b, a) = self.pixels[i as usize].to_rgba8();
      *pixel = image::Rgba([r, g, b, a]);
    }

    imgbuf
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum PaintCommand {
  FillColor(CSSColor, Rect),
}

pub type PaintList = Vec<PaintCommand>;

pub fn build_paint_list(layout_root: &LayoutBox) -> PaintList {
  let mut list = vec![];
  paint_layout_box(&mut list, layout_root);
  list
}

pub fn paint_layout_box(list: &mut PaintList, layout_box: &LayoutBox) {
  paint_background(list, layout_box);
  paint_borders(list, layout_box);
  for child in &layout_box.children {
    paint_layout_box(list, &*child.borrow())
  }
}

pub fn paint_background(list: &mut PaintList, layout_box: &LayoutBox) {
  if let Some(ref color) = layout_box.get_styled_node().and_then(|s| {
    s.borrow().declaration_nth_components_map(
      &[("background", 0), ("background-color", 0)],
      CSSComponentValue::to_color,
    )
  }) {
    list.push(PaintCommand::FillColor(
      color.clone(),
      layout_box.dimensions.border_box(),
    ))
  }
}

pub fn paint_borders(list: &mut PaintList, layout_box: &LayoutBox) {
  if let Some(ref color) = layout_box.get_styled_node().and_then(|s| {
    s.borrow().declaration_nth_components_map(
      &[("border", 0), ("border-color", 0)],
      CSSComponentValue::to_color,
    )
  }) {
    let d = &layout_box.dimensions;
    let border_box = d.border_box();

    list.push(PaintCommand::FillColor(
      color.clone(),
      Rect {
        x: border_box.x,
        y: border_box.y,
        width: d.border.left,
        height: border_box.height,
      },
    ));

    list.push(PaintCommand::FillColor(
      color.clone(),
      Rect {
        x: border_box.x + border_box.width - d.border.right,
        y: border_box.y,
        width: d.border.right,
        height: border_box.height,
      },
    ));

    list.push(PaintCommand::FillColor(
      color.clone(),
      Rect {
        x: border_box.x,
        y: border_box.y,
        width: border_box.width,
        height: d.border.top,
      },
    ));

    list.push(PaintCommand::FillColor(
      color.clone(),
      Rect {
        x: border_box.x,
        y: border_box.y + border_box.height - d.border.bottom,
        width: border_box.width,
        height: d.border.bottom,
      },
    ));
  }
}

trait Clamp {
  fn clamp(self, lower: Self, upper: Self) -> Self;
}

impl Clamp for f32 {
  fn clamp(self, lower: f32, upper: f32) -> f32 {
    self.max(lower).min(upper)
  }
}
