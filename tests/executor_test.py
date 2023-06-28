import quickstrom.executor as executor

def test_remote_options():
    caps = {
        "foo": 1,
        "bar": 2,
        "baz": { "qux": 3 }
    }
    opts = executor.RemoteOptions(caps)

    assert opts.to_capabilities == caps