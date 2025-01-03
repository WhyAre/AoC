use rayon::iter::{IntoParallelIterator, ParallelIterator};
use reqwest::cookie::Jar;
use reqwest::Url;
use std::fs::File;
use std::path::Path;
use std::sync::Arc;
use std::{env, io};

// Configuration
const OUTPUT_DIR: &str = "inputs";
const YEAR: i32 = 2024;

fn main() {
    // Cookie name: "session"
    let session_id = env::var("SESSION_ID").expect("Cannot find SESSION_ID env var");
    let cookie = format!("session={session_id}");

    // Example URL: https://adventofcode.com/2024/day/8/input
    (1..=25)
        .into_par_iter()
        .filter_map(|day| {
            let url: Url = format!("https://adventofcode.com/{}/day/{}/input", YEAR, day)
                .parse()
                .expect("Failed to parse URL");

            let jar = Jar::default();
            jar.add_cookie_str(cookie.as_str(), &url);
            let client = reqwest::blocking::Client::builder()
                .cookie_store(true)
                .cookie_provider(Arc::new(jar))
                .build();

            client
                .and_then(|client| client.get(url).send())
                .map(|resp| (day, resp))
                .ok()
        })
        .for_each(|(day, mut resp)| {
            let output_path = Path::new(".")
                .join(OUTPUT_DIR)
                .join(format!("day{}.txt", day));
            let mut out = File::create(output_path).expect("failed to create file");
            io::copy(&mut resp, &mut out).expect("failed to copy content");
            println!("Downloaded: {day}");
        });
}
