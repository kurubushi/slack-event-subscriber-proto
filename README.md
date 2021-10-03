# slack-event-subscriber-proto

練習用。
Event subscription を受け取れる Lambda function を作ってみる。

## インフラ

terraform の state を安置する S3 bucket を用意し `terraform/provider.tf` の `backend` を書き換える。

シークレットを保存するための Secret Manager も事前に用意しておく。ここでは `for-bot` という名前を仮定している。

terraform コマンドを用意する。

```
$ asdf plugin-add terraform
$ asdf intall terraform
```

apply する。

```
$ cd terraform
$ terraform init
$ terraform apply
```

## Lambda function

terraform で作った空 Lambda function に実行化ファイルを埋め込む。

```
$ cd lambda
$ make deploy
```
