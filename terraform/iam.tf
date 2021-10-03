# Lambda 用ロール
resource "aws_iam_role" "bot-lambda" {
  name               = "bot-lambda"
  assume_role_policy = data.aws_iam_policy_document.bot-lambda-policy.json

  inline_policy {
    name   = "read-secrets-manager"
    policy = data.aws_iam_policy_document.secrets-manager-read-policy.json
  }
}

data "aws_iam_policy_document" "bot-lambda-policy" {
  statement {
    effect  = "Allow"
    actions = ["sts:AssumeRole"]
    principals {
      type        = "Service"
      identifiers = ["lambda.amazonaws.com"]
    }
  }
}

data "aws_iam_policy_document" "secrets-manager-read-policy" {
  statement {
    effect    = "Allow"
    actions   = ["secretsmanager:GetSecretValue"]
    resources = [data.aws_secretsmanager_secret.for-bot.arn]
  }
}
