# API Gateway
resource "aws_api_gateway_rest_api" "bot" {
  name = "bot"
}

resource "aws_api_gateway_resource" "events" {
  path_part   = "events"
  parent_id   = aws_api_gateway_rest_api.bot.root_resource_id
  rest_api_id = aws_api_gateway_rest_api.bot.id
}

resource "aws_api_gateway_method" "post" {
  rest_api_id   = aws_api_gateway_rest_api.bot.id
  resource_id   = aws_api_gateway_resource.events.id
  http_method   = "POST"
  authorization = "NONE"
}

resource "aws_api_gateway_integration" "with-lambda" {
  rest_api_id             = aws_api_gateway_rest_api.bot.id
  resource_id             = aws_api_gateway_resource.events.id
  http_method             = aws_api_gateway_method.post.http_method
  integration_http_method = "POST"
  type                    = "AWS_PROXY"
  uri                     = aws_lambda_function.bot.invoke_arn
}

resource "aws_api_gateway_deployment" "production" {
  rest_api_id = aws_api_gateway_rest_api.bot.id
  stage_name  = "production"

  lifecycle {
    create_before_destroy = true
  }

  depends_on = [
    # avoid error:
    # Error creating API Gateway Deployment: BadRequestException: The REST API doesn't contain any methods
    aws_api_gateway_method.post,
    # avoid error:
    # Error creating API Gateway Deployment: BadRequestException: No integration defined for method
    aws_api_gateway_integration.with-lambda,
  ]
}

output "endpoint" {
  description = "Endpoint of bot"

  value = "${aws_api_gateway_deployment.production.invoke_url}/${aws_api_gateway_resource.events.path_part}"
}

