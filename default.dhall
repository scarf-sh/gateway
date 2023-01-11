{ origin = env:ORIGIN as Text ? "unknown"
, sync-interval = +5
, mock-s3-endpoint = Some (env:MOCK_S3_ENDPOINT as Text ? "")
, manifest-bucket = env:SCARF_MANIFEST_BUCKET as Text
, manifest-object-key = "manifest.json"
, max-open-proxy-connections = +20
, domains-to-proxy-through-internal-docker-caching-registries =
  [ { mapKey = "docker.io"
    , mapValue = "container-registry-dockerhub.default.svc.cluster.local"
    }
  , { mapKey = "registry-1.docker.io"
    , mapValue = "container-registry-dockerhub.default.svc.cluster.local"
    }
  ]
}
