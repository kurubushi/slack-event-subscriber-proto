provider "aws" {
  region = "ap-northeast-1"
}

terraform {
  required_version = "1.1.6"

  backend "s3" {
    region = "ap-northeast-1"
    bucket = "test"
    key    = "bot/tfstate"
  }
}
