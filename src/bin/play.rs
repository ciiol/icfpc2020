extern crate icfpc2020;

use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;

use icfpc2020::ast;
use icfpc2020::interactor::{self, Interactor, Point};
use icfpc2020::modulator::{self, Modulatable};
use icfpc2020::parser::parse;
use icfpc2020::solver::{self, Solver};

use pixels::{wgpu::Surface, Error, Pixels, SurfaceTexture};
use winit::dpi::LogicalSize;
use winit::event::{Event, VirtualKeyCode};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::WindowBuilder;
use winit_input_helper::WinitInputHelper;

const WIDTH: u32 = 64;
const HEIGHT: u32 = 64;

/// Representation of the application state. In this example, a box will bounce around the screen.
struct Renderer {
    interactor: Interactor,
    points: HashMap<Point, bool>
}

fn main() -> Result<(), Error> {
    let event_loop = EventLoop::new();
    let mut input = WinitInputHelper::new();
    let window = {
        let size = LogicalSize::new(WIDTH as f64, HEIGHT as f64);
        WindowBuilder::new()
            .with_title("Play with aliens")
            .with_inner_size(size)
            .with_min_inner_size(size)
            .build(&event_loop)
            .unwrap()
    };

    let mut pixels = {
        let surface = Surface::create(&window);
        let surface_texture = SurfaceTexture::new(WIDTH, HEIGHT, surface);
        Pixels::new(WIDTH, HEIGHT, surface_texture)?
    };

    let args: Vec<String> = env::args().collect();

    let rules_file_name = &args[1];
    let entry_text = &args[2];

    let rules_file = File::open(rules_file_name).unwrap();
    let rules = io::BufReader::new(rules_file)
        .lines()
        .map(|s| solver::Rule::from(ast::build_define_tree(&parse(&s.unwrap())).unwrap()));

    let mut solver = Solver::new();
    for rule in rules {
        solver.add_rule(rule).unwrap();
    }

    let entry = match ast::build_subtree(&parse(entry_text)).unwrap() {
        (Some(entry), []) => entry,
        (_entry, tail) => panic!("Unexpected tail {:?}", tail),
    };

    let interactor = Interactor::new(solver, solver::Value::from(entry));
    let mut renderer = Renderer::new(interactor);
    renderer.apply_click(Point::new(0, 0));

    event_loop.run(move |event, _, control_flow| {
        if let Event::RedrawRequested(_) = event {
            renderer.draw(pixels.get_frame());
            if pixels
                .render()
                .map_err(|e| panic!("pixels.render() failed: {}", e))
                .is_err()
            {
                *control_flow = ControlFlow::Exit;
                return;
            }
        }

        if input.update(&event) {
            if input.key_pressed(VirtualKeyCode::Escape) || input.quit() {
                *control_flow = ControlFlow::Exit;
                return;
            }

            if let Some(size) = input.window_resized() {
                pixels.resize(size.width, size.height);
            }

            if input.mouse_pressed(0) {
                let (x, y) = input
                    .mouse()
                    .map(|pos| {
                        pixels
                            .window_pos_to_pixel(pos)
                            .unwrap_or_else(|pos| pixels.clamp_pixel_pos(pos))
                    })
                    .unwrap_or_default();
                renderer.apply_click(from_screen(Point::new(x as i64, y as i64)));
                window.request_redraw();
            }
        }
    });
}

impl Renderer {
    fn new(interactor: Interactor) -> Self {
        Self { interactor, points: HashMap::new() }
    }

    fn apply_click(&mut self, point: Point) {
        println!("click to {}", point);
        match self.interactor.apply(point) {
            interactor::Result::Draw(points) => {
                println!("clear");
                self.points = HashMap::new();
                for p in points.into_iter() {
                    println!("add point {}", p);
                    self.points.insert(to_screen(p), true);
                }
            }
            interactor::Result::Send(val) => {
                let request = modulator::modulate(&Modulatable::from(val));
                println!("need to send {}", request);
                self.points = HashMap::new();
            }
        }
    }

    fn draw(&self, frame: &mut [u8]) {
        for (i, pixel) in frame.chunks_exact_mut(4).enumerate() {
            let x = (i % WIDTH as usize) as i64;
            let y = (i / WIDTH as usize) as i64;
            let p = Point::new(x, y);

            let rgba = if let Some(_) = self.points.get(&p) {
                [0x00, 0x00, 0x00, 0xff]
            } else {
                [0xff, 0xff, 0xff, 0xff]
            };

            pixel.copy_from_slice(&rgba);
        }
    }
}

fn to_screen(p: Point) -> Point {
    Point::new(p.x + (WIDTH as i64) / 2, p.y + (HEIGHT as i64) / 2)
}

fn from_screen(p: Point) -> Point {
    Point::new(p.x - (WIDTH as i64) / 2, p.y - (HEIGHT as i64) / 2)
}