use icfpc2020::modulator::{demodulate, modulate, Modulatable};
use std::env;
use std::io::Error;

fn main() -> Result<(), Error> {
    let args: Vec<String> = env::args().collect();

    let server_url = &args[1];
    let player_key = &args[2];

    println!("ServerUrl: {}; PlayerKey: {}", server_url, player_key);

    let uri = build_api_uri(&server_url);

    let join_request = build_join_request(&player_key);
    let join_response = isahc::post(uri, join_request)?;

    println!("Response: {:?}", join_response.body());

    assert!(join_response.status().is_success());

    Ok(())
}

fn build_api_uri(server_url: &str) -> String {
    let api_key = option_env!("API_KEY").unwrap();
    format!("{}/aliens/send?apiKey={}", server_url, api_key)
}

fn build_join_request(player_key: &str) -> String {
    let player_key_int = player_key.parse::<i64>().unwrap();

    modulate(&Modulatable::Pair(
        Box::new(Modulatable::Num(2)),
        Box::new(Modulatable::Pair(
            Box::new(Modulatable::Num(player_key_int)),
            Box::new(Modulatable::Pair(
                Box::new(Modulatable::Nil),
                Box::new(Modulatable::Nil),
            )),
        )),
    ))
}
