# dummy Lambda function
resource "aws_lambda_function" "bot" {
  function_name = "bot"
  role          = aws_iam_role.bot-lambda.arn
  runtime       = "provided.al2"
  filename      = data.archive_file.dummy-impl.output_path
  handler       = "main"

  lifecycle {
    ignore_changes = [
      filename
    ]
  }
}

data "archive_file" "dummy-impl" {
  type        = "zip"
  output_path = "${path.module}/dummy-impl.zip"
  source {
    content  = "dummy"
    filename = "main"
  }
}

resource "aws_lambda_permission" "bot" {
  action        = "lambda:InvokeFunction"
  function_name = aws_lambda_function.bot.function_name
  principal     = "apigateway.amazonaws.com"
  source_arn    = "${aws_api_gateway_rest_api.bot.execution_arn}/*/*/*"
}
