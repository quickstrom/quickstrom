CLOUDFRONT_DISTRIBUTION_ID=E2JPJK66BWKPCH

deploy:
	aws s3 sync "src/" s3://webcheck.tools
	aws cloudfront create-invalidation --distribution-id $(CLOUDFRONT_DISTRIBUTION_ID) --paths '/*' --profile webcheck.tools

.PHONY: deploy
