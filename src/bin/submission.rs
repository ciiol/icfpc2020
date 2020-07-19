use icfpc2020::modulator::{demodulate, modulate, Modulatable};
use isahc::prelude::*;
use std::env;
use std::io::Error;
use std::str::FromStr;

fn main() -> Result<(), Error> {
    let args: Vec<String> = env::args().collect();

    let server_url = &args[1];
    let player_key = &args[2];

    let player_key_int = i64::from_str(player_key).unwrap();

    println!("ServerUrl: {}; PlayerKey: {}", server_url, player_key);

    let uri = build_api_uri(&server_url);

    let join_request = build_join_request(player_key_int);
    let mut join_response = isahc::post(&uri, join_request)?;

    assert!(join_response.status().is_success());
    println!("JOIN Response: {:?}", demodulate(&join_response.text()?));

    let start_request = build_start_request(player_key_int);
    let mut start_response = isahc::post(&uri, start_request)?;

    assert!(start_response.status().is_success());
    println!("START Response: {:?}", demodulate(&start_response.text()?));

    Ok(())
}

fn build_api_uri(server_url: &str) -> String {
    let api_key = option_env!("API_KEY").unwrap();
    format!("{}/aliens/send?apiKey={}", server_url, api_key)
}

fn build_join_request(player_key: i64) -> String {
    modulate(&Modulatable::Pair(
        Box::new(Modulatable::Num(2)),
        Box::new(Modulatable::Pair(
            Box::new(Modulatable::Num(player_key)),
            Box::new(Modulatable::Pair(
                Box::new(Modulatable::Nil),
                Box::new(Modulatable::Nil),
            )),
        )),
    ))
}

fn build_start_request(player_key: i64) -> String {
    modulate(&Modulatable::Pair(
        Box::new(Modulatable::Num(3)),
        Box::new(Modulatable::Pair(
            Box::new(Modulatable::Num(player_key)),
            Box::new(Modulatable::Pair(
                // TODO: add ship characterstics
                Box::new(Modulatable::Nil),
                Box::new(Modulatable::Nil),
            )),
        )),
    ))
}
