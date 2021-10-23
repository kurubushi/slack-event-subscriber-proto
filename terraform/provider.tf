provider "aws" {
  region = "ap-northeast-1"
}

terraform {
  required_version = "1.0.9"

  backend "s3" {
    region = "ap-northeast-1"
    bucket = "test"
    key    = "bot/tfstate"
  }
}
