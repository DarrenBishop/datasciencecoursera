library(httr)
library(jsonlite)

oauth_endpoints("github")

github_app = oauth_app("github", key="3eca9fbebc58a0113ddd", secret="6ad956049ac8d00cfc94547a4b5ad46514067ad4")

github_token = oauth2.0_token(oauth_endpoints("github"), github_app)

github_config = config(token = github_token)

response = GET("https://api.github.com/users/jtleek/repos", github_config)

stop_for_status(response)

df = fromJSON(toJSON(content(response)))

print(df[df$name == "datasharing", c("name", "created_at")])
