use shilipo::{
  css::parse::parse_stylesheet,
  html::parse::parse_html,
  layout::{
    layout_box::{Dimensions, EdgeSizes, Rect},
    layout_tree::layout_tree,
  },
  painting::{
    graphics::State as GraphicsState,
    paint::{build_paint_list, Canvas},
  },
  style::styled_node::style_tree,
};
use winit::{
  event::{ElementState, Event, KeyboardInput, VirtualKeyCode, WindowEvent},
  event_loop::{ControlFlow, EventLoop},
  window::WindowBuilder,
};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
  tracing_subscriber::fmt::init();
  let event_loop = EventLoop::new();
  let window = WindowBuilder::new().build(&event_loop).unwrap();
  window.set_title("shilipo");

  let html = r#"
    <!DOCTYPE html SYSTEM "about:legacy-compat">
    <html>
      <head>
        <meta charset="utf-8" />
        <title>github.com</title>
      </head>
      <body>
        <div class="a"></div>
        <div class="b"></div>
      </body>
    </html>
  "#;

  let css = r#"
    .a {
        width: 100px;
        height: 50px;
        margin: auto;
        background: #FF0000;
    }
    
    .b {
        width: 200px;
        margin-left: 0;
        margin-right: auto;
        height: 100px;
        background: #00FFFF;
    }
  "#;

  let (_, dom) = parse_html(html).unwrap();
  let (_, cssom) = parse_stylesheet(css).unwrap();
  let styled_node = style_tree(dom.unwrap(), &cssom);

  let generate_canvas = move |width: u32, height: u32| -> Canvas {
    let root_dimension = Dimensions {
      content: Rect {
        x: 0f32,
        y: 0f32,
        width: width as f32,
        height: height as f32,
      },
      padding: EdgeSizes::default(),
      border: EdgeSizes::default(),
      margin: EdgeSizes::default(),
    };
    let layout_root = layout_tree(styled_node.clone(), root_dimension);
    let paint_list = build_paint_list(&*layout_root.borrow());
    let mut canvas = Canvas::new(width as usize, height as usize);
    for pc in &paint_list {
      canvas.paint_item(pc);
    }
    canvas
  };

  let mut state = GraphicsState::new(&window, generate_canvas).await;

  event_loop.run(move |event, _, control_flow| match event {
    Event::WindowEvent {
      ref event,
      window_id,
    } if window_id == window.id() => match event {
      WindowEvent::CloseRequested
      | WindowEvent::KeyboardInput {
        input:
          KeyboardInput {
            state: ElementState::Pressed,
            virtual_keycode: Some(VirtualKeyCode::Escape),
            ..
          },
        ..
      } => *control_flow = ControlFlow::Exit,
      WindowEvent::Resized(physical_size) => state.resize(*physical_size),
      WindowEvent::ScaleFactorChanged { new_inner_size, .. } => state.resize(**new_inner_size),
      _ => {}
    },
    Event::RedrawRequested(window_id) if window_id == window.id() => {
      state.update();
      match state.render() {
        Ok(_) => {}
        Err(wgpu::SurfaceError::Lost) => state.resize(state.size),
        Err(wgpu::SurfaceError::OutOfMemory) => *control_flow = ControlFlow::Exit,
        Err(e) => eprintln!("{:?}", e),
      }
    }
    Event::MainEventsCleared => window.request_redraw(),
    _ => {}
  })
}
