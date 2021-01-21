{ fetchurl, fetchgit, linkFarm, runCommandNoCC, gnutar }: rec {
  offline_cache = linkFarm "offline" packages;
  packages = [
    {
      name = "https___registry.npmjs.org__babel_code_frame___code_frame_7.10.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_code_frame___code_frame_7.10.4.tgz";
        url  = "https://registry.npmjs.org/@babel/code-frame/-/code-frame-7.10.4.tgz";
        sha1 = "168da1a36e90da68ae8d49c0f1b48c7c6249213a";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_compat_data___compat_data_7.12.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_compat_data___compat_data_7.12.7.tgz";
        url  = "https://registry.npmjs.org/@babel/compat-data/-/compat-data-7.12.7.tgz";
        sha1 = "9329b4782a7d6bbd7eef57e11addf91ee3ef1e41";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_core___core_7.12.9.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_core___core_7.12.9.tgz";
        url  = "https://registry.npmjs.org/@babel/core/-/core-7.12.9.tgz";
        sha1 = "fd450c4ec10cdbb980e2928b7aa7a28484593fc8";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_generator___generator_7.12.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_generator___generator_7.12.5.tgz";
        url  = "https://registry.npmjs.org/@babel/generator/-/generator-7.12.5.tgz";
        sha1 = "a2c50de5c8b6d708ab95be5e6053936c1884a4de";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_annotate_as_pure___helper_annotate_as_pure_7.10.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_annotate_as_pure___helper_annotate_as_pure_7.10.4.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-annotate-as-pure/-/helper-annotate-as-pure-7.10.4.tgz";
        sha1 = "5bf0d495a3f757ac3bda48b5bf3b3ba309c72ba3";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_builder_binary_assignment_operator_visitor___helper_builder_binary_assignment_operator_visitor_7.10.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_builder_binary_assignment_operator_visitor___helper_builder_binary_assignment_operator_visitor_7.10.4.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-builder-binary-assignment-operator-visitor/-/helper-builder-binary-assignment-operator-visitor-7.10.4.tgz";
        sha1 = "bb0b75f31bf98cbf9ff143c1ae578b87274ae1a3";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_builder_react_jsx_experimental___helper_builder_react_jsx_experimental_7.12.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_builder_react_jsx_experimental___helper_builder_react_jsx_experimental_7.12.4.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-builder-react-jsx-experimental/-/helper-builder-react-jsx-experimental-7.12.4.tgz";
        sha1 = "55fc1ead5242caa0ca2875dcb8eed6d311e50f48";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_builder_react_jsx___helper_builder_react_jsx_7.10.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_builder_react_jsx___helper_builder_react_jsx_7.10.4.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-builder-react-jsx/-/helper-builder-react-jsx-7.10.4.tgz";
        sha1 = "8095cddbff858e6fa9c326daee54a2f2732c1d5d";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_compilation_targets___helper_compilation_targets_7.12.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_compilation_targets___helper_compilation_targets_7.12.5.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-compilation-targets/-/helper-compilation-targets-7.12.5.tgz";
        sha1 = "cb470c76198db6a24e9dbc8987275631e5d29831";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_create_class_features_plugin___helper_create_class_features_plugin_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_create_class_features_plugin___helper_create_class_features_plugin_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-create-class-features-plugin/-/helper-create-class-features-plugin-7.12.1.tgz";
        sha1 = "3c45998f431edd4a9214c5f1d3ad1448a6137f6e";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_create_regexp_features_plugin___helper_create_regexp_features_plugin_7.12.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_create_regexp_features_plugin___helper_create_regexp_features_plugin_7.12.7.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-create-regexp-features-plugin/-/helper-create-regexp-features-plugin-7.12.7.tgz";
        sha1 = "2084172e95443fa0a09214ba1bb328f9aea1278f";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_define_map___helper_define_map_7.10.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_define_map___helper_define_map_7.10.5.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-define-map/-/helper-define-map-7.10.5.tgz";
        sha1 = "b53c10db78a640800152692b13393147acb9bb30";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_explode_assignable_expression___helper_explode_assignable_expression_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_explode_assignable_expression___helper_explode_assignable_expression_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-explode-assignable-expression/-/helper-explode-assignable-expression-7.12.1.tgz";
        sha1 = "8006a466695c4ad86a2a5f2fb15b5f2c31ad5633";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_function_name___helper_function_name_7.10.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_function_name___helper_function_name_7.10.4.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-function-name/-/helper-function-name-7.10.4.tgz";
        sha1 = "d2d3b20c59ad8c47112fa7d2a94bc09d5ef82f1a";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_get_function_arity___helper_get_function_arity_7.10.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_get_function_arity___helper_get_function_arity_7.10.4.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-get-function-arity/-/helper-get-function-arity-7.10.4.tgz";
        sha1 = "98c1cbea0e2332f33f9a4661b8ce1505b2c19ba2";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_hoist_variables___helper_hoist_variables_7.10.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_hoist_variables___helper_hoist_variables_7.10.4.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-hoist-variables/-/helper-hoist-variables-7.10.4.tgz";
        sha1 = "d49b001d1d5a68ca5e6604dda01a6297f7c9381e";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_member_expression_to_functions___helper_member_expression_to_functions_7.12.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_member_expression_to_functions___helper_member_expression_to_functions_7.12.7.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-member-expression-to-functions/-/helper-member-expression-to-functions-7.12.7.tgz";
        sha1 = "aa77bd0396ec8114e5e30787efa78599d874a855";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_module_imports___helper_module_imports_7.12.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_module_imports___helper_module_imports_7.12.5.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-module-imports/-/helper-module-imports-7.12.5.tgz";
        sha1 = "1bfc0229f794988f76ed0a4d4e90860850b54dfb";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_module_transforms___helper_module_transforms_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_module_transforms___helper_module_transforms_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-module-transforms/-/helper-module-transforms-7.12.1.tgz";
        sha1 = "7954fec71f5b32c48e4b303b437c34453fd7247c";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_optimise_call_expression___helper_optimise_call_expression_7.12.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_optimise_call_expression___helper_optimise_call_expression_7.12.7.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-optimise-call-expression/-/helper-optimise-call-expression-7.12.7.tgz";
        sha1 = "7f94ae5e08721a49467346aa04fd22f750033b9c";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_plugin_utils___helper_plugin_utils_7.10.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_plugin_utils___helper_plugin_utils_7.10.4.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-plugin-utils/-/helper-plugin-utils-7.10.4.tgz";
        sha1 = "2f75a831269d4f677de49986dff59927533cf375";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_remap_async_to_generator___helper_remap_async_to_generator_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_remap_async_to_generator___helper_remap_async_to_generator_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-remap-async-to-generator/-/helper-remap-async-to-generator-7.12.1.tgz";
        sha1 = "8c4dbbf916314f6047dc05e6a2217074238347fd";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_replace_supers___helper_replace_supers_7.12.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_replace_supers___helper_replace_supers_7.12.5.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-replace-supers/-/helper-replace-supers-7.12.5.tgz";
        sha1 = "f009a17543bbbbce16b06206ae73b63d3fca68d9";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_simple_access___helper_simple_access_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_simple_access___helper_simple_access_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-simple-access/-/helper-simple-access-7.12.1.tgz";
        sha1 = "32427e5aa61547d38eb1e6eaf5fd1426fdad9136";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_skip_transparent_expression_wrappers___helper_skip_transparent_expression_wrappers_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_skip_transparent_expression_wrappers___helper_skip_transparent_expression_wrappers_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-skip-transparent-expression-wrappers/-/helper-skip-transparent-expression-wrappers-7.12.1.tgz";
        sha1 = "462dc63a7e435ade8468385c63d2b84cce4b3cbf";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_split_export_declaration___helper_split_export_declaration_7.11.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_split_export_declaration___helper_split_export_declaration_7.11.0.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-split-export-declaration/-/helper-split-export-declaration-7.11.0.tgz";
        sha1 = "f8a491244acf6a676158ac42072911ba83ad099f";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_validator_identifier___helper_validator_identifier_7.10.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_validator_identifier___helper_validator_identifier_7.10.4.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-validator-identifier/-/helper-validator-identifier-7.10.4.tgz";
        sha1 = "a78c7a7251e01f616512d31b10adcf52ada5e0d2";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_validator_option___helper_validator_option_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_validator_option___helper_validator_option_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-validator-option/-/helper-validator-option-7.12.1.tgz";
        sha1 = "175567380c3e77d60ff98a54bb015fe78f2178d9";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_wrap_function___helper_wrap_function_7.12.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_wrap_function___helper_wrap_function_7.12.3.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-wrap-function/-/helper-wrap-function-7.12.3.tgz";
        sha1 = "3332339fc4d1fbbf1c27d7958c27d34708e990d9";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helpers___helpers_7.12.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helpers___helpers_7.12.5.tgz";
        url  = "https://registry.npmjs.org/@babel/helpers/-/helpers-7.12.5.tgz";
        sha1 = "1a1ba4a768d9b58310eda516c449913fe647116e";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_highlight___highlight_7.10.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_highlight___highlight_7.10.4.tgz";
        url  = "https://registry.npmjs.org/@babel/highlight/-/highlight-7.10.4.tgz";
        sha1 = "7d1bdfd65753538fabe6c38596cdb76d9ac60143";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_parser___parser_7.12.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_parser___parser_7.12.7.tgz";
        url  = "https://registry.npmjs.org/@babel/parser/-/parser-7.12.7.tgz";
        sha1 = "fee7b39fe809d0e73e5b25eecaf5780ef3d73056";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_proposal_async_generator_functions___plugin_proposal_async_generator_functions_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_proposal_async_generator_functions___plugin_proposal_async_generator_functions_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-proposal-async-generator-functions/-/plugin-proposal-async-generator-functions-7.12.1.tgz";
        sha1 = "dc6c1170e27d8aca99ff65f4925bd06b1c90550e";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_proposal_class_properties___plugin_proposal_class_properties_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_proposal_class_properties___plugin_proposal_class_properties_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-proposal-class-properties/-/plugin-proposal-class-properties-7.12.1.tgz";
        sha1 = "a082ff541f2a29a4821065b8add9346c0c16e5de";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_proposal_dynamic_import___plugin_proposal_dynamic_import_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_proposal_dynamic_import___plugin_proposal_dynamic_import_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-proposal-dynamic-import/-/plugin-proposal-dynamic-import-7.12.1.tgz";
        sha1 = "43eb5c2a3487ecd98c5c8ea8b5fdb69a2749b2dc";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_proposal_export_namespace_from___plugin_proposal_export_namespace_from_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_proposal_export_namespace_from___plugin_proposal_export_namespace_from_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-proposal-export-namespace-from/-/plugin-proposal-export-namespace-from-7.12.1.tgz";
        sha1 = "8b9b8f376b2d88f5dd774e4d24a5cc2e3679b6d4";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_proposal_json_strings___plugin_proposal_json_strings_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_proposal_json_strings___plugin_proposal_json_strings_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-proposal-json-strings/-/plugin-proposal-json-strings-7.12.1.tgz";
        sha1 = "d45423b517714eedd5621a9dfdc03fa9f4eb241c";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_proposal_logical_assignment_operators___plugin_proposal_logical_assignment_operators_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_proposal_logical_assignment_operators___plugin_proposal_logical_assignment_operators_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-proposal-logical-assignment-operators/-/plugin-proposal-logical-assignment-operators-7.12.1.tgz";
        sha1 = "f2c490d36e1b3c9659241034a5d2cd50263a2751";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_proposal_nullish_coalescing_operator___plugin_proposal_nullish_coalescing_operator_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_proposal_nullish_coalescing_operator___plugin_proposal_nullish_coalescing_operator_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-proposal-nullish-coalescing-operator/-/plugin-proposal-nullish-coalescing-operator-7.12.1.tgz";
        sha1 = "3ed4fff31c015e7f3f1467f190dbe545cd7b046c";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_proposal_numeric_separator___plugin_proposal_numeric_separator_7.12.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_proposal_numeric_separator___plugin_proposal_numeric_separator_7.12.7.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-proposal-numeric-separator/-/plugin-proposal-numeric-separator-7.12.7.tgz";
        sha1 = "8bf253de8139099fea193b297d23a9d406ef056b";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_proposal_object_rest_spread___plugin_proposal_object_rest_spread_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_proposal_object_rest_spread___plugin_proposal_object_rest_spread_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-proposal-object-rest-spread/-/plugin-proposal-object-rest-spread-7.12.1.tgz";
        sha1 = "def9bd03cea0f9b72283dac0ec22d289c7691069";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_proposal_optional_catch_binding___plugin_proposal_optional_catch_binding_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_proposal_optional_catch_binding___plugin_proposal_optional_catch_binding_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-proposal-optional-catch-binding/-/plugin-proposal-optional-catch-binding-7.12.1.tgz";
        sha1 = "ccc2421af64d3aae50b558a71cede929a5ab2942";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_proposal_optional_chaining___plugin_proposal_optional_chaining_7.12.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_proposal_optional_chaining___plugin_proposal_optional_chaining_7.12.7.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-proposal-optional-chaining/-/plugin-proposal-optional-chaining-7.12.7.tgz";
        sha1 = "e02f0ea1b5dc59d401ec16fb824679f683d3303c";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_proposal_private_methods___plugin_proposal_private_methods_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_proposal_private_methods___plugin_proposal_private_methods_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-proposal-private-methods/-/plugin-proposal-private-methods-7.12.1.tgz";
        sha1 = "86814f6e7a21374c980c10d38b4493e703f4a389";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_proposal_unicode_property_regex___plugin_proposal_unicode_property_regex_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_proposal_unicode_property_regex___plugin_proposal_unicode_property_regex_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-proposal-unicode-property-regex/-/plugin-proposal-unicode-property-regex-7.12.1.tgz";
        sha1 = "2a183958d417765b9eae334f47758e5d6a82e072";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_async_generators___plugin_syntax_async_generators_7.8.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_async_generators___plugin_syntax_async_generators_7.8.4.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-async-generators/-/plugin-syntax-async-generators-7.8.4.tgz";
        sha1 = "a983fb1aeb2ec3f6ed042a210f640e90e786fe0d";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_bigint___plugin_syntax_bigint_7.8.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_bigint___plugin_syntax_bigint_7.8.3.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-bigint/-/plugin-syntax-bigint-7.8.3.tgz";
        sha1 = "4c9a6f669f5d0cdf1b90a1671e9a146be5300cea";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_class_properties___plugin_syntax_class_properties_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_class_properties___plugin_syntax_class_properties_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-class-properties/-/plugin-syntax-class-properties-7.12.1.tgz";
        sha1 = "bcb297c5366e79bebadef509549cd93b04f19978";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_dynamic_import___plugin_syntax_dynamic_import_7.8.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_dynamic_import___plugin_syntax_dynamic_import_7.8.3.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-dynamic-import/-/plugin-syntax-dynamic-import-7.8.3.tgz";
        sha1 = "62bf98b2da3cd21d626154fc96ee5b3cb68eacb3";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_export_namespace_from___plugin_syntax_export_namespace_from_7.8.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_export_namespace_from___plugin_syntax_export_namespace_from_7.8.3.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-export-namespace-from/-/plugin-syntax-export-namespace-from-7.8.3.tgz";
        sha1 = "028964a9ba80dbc094c915c487ad7c4e7a66465a";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_flow___plugin_syntax_flow_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_flow___plugin_syntax_flow_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-flow/-/plugin-syntax-flow-7.12.1.tgz";
        sha1 = "a77670d9abe6d63e8acadf4c31bb1eb5a506bbdd";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_import_meta___plugin_syntax_import_meta_7.10.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_import_meta___plugin_syntax_import_meta_7.10.4.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-import-meta/-/plugin-syntax-import-meta-7.10.4.tgz";
        sha1 = "ee601348c370fa334d2207be158777496521fd51";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_json_strings___plugin_syntax_json_strings_7.8.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_json_strings___plugin_syntax_json_strings_7.8.3.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-json-strings/-/plugin-syntax-json-strings-7.8.3.tgz";
        sha1 = "01ca21b668cd8218c9e640cb6dd88c5412b2c96a";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_jsx___plugin_syntax_jsx_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_jsx___plugin_syntax_jsx_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-jsx/-/plugin-syntax-jsx-7.12.1.tgz";
        sha1 = "9d9d357cc818aa7ae7935917c1257f67677a0926";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_logical_assignment_operators___plugin_syntax_logical_assignment_operators_7.10.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_logical_assignment_operators___plugin_syntax_logical_assignment_operators_7.10.4.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-logical-assignment-operators/-/plugin-syntax-logical-assignment-operators-7.10.4.tgz";
        sha1 = "ca91ef46303530448b906652bac2e9fe9941f699";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_nullish_coalescing_operator___plugin_syntax_nullish_coalescing_operator_7.8.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_nullish_coalescing_operator___plugin_syntax_nullish_coalescing_operator_7.8.3.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-nullish-coalescing-operator/-/plugin-syntax-nullish-coalescing-operator-7.8.3.tgz";
        sha1 = "167ed70368886081f74b5c36c65a88c03b66d1a9";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_numeric_separator___plugin_syntax_numeric_separator_7.10.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_numeric_separator___plugin_syntax_numeric_separator_7.10.4.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-numeric-separator/-/plugin-syntax-numeric-separator-7.10.4.tgz";
        sha1 = "b9b070b3e33570cd9fd07ba7fa91c0dd37b9af97";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_object_rest_spread___plugin_syntax_object_rest_spread_7.8.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_object_rest_spread___plugin_syntax_object_rest_spread_7.8.3.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-object-rest-spread/-/plugin-syntax-object-rest-spread-7.8.3.tgz";
        sha1 = "60e225edcbd98a640332a2e72dd3e66f1af55871";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_optional_catch_binding___plugin_syntax_optional_catch_binding_7.8.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_optional_catch_binding___plugin_syntax_optional_catch_binding_7.8.3.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-optional-catch-binding/-/plugin-syntax-optional-catch-binding-7.8.3.tgz";
        sha1 = "6111a265bcfb020eb9efd0fdfd7d26402b9ed6c1";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_optional_chaining___plugin_syntax_optional_chaining_7.8.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_optional_chaining___plugin_syntax_optional_chaining_7.8.3.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-optional-chaining/-/plugin-syntax-optional-chaining-7.8.3.tgz";
        sha1 = "4f69c2ab95167e0180cd5336613f8c5788f7d48a";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_top_level_await___plugin_syntax_top_level_await_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_top_level_await___plugin_syntax_top_level_await_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-top-level-await/-/plugin-syntax-top-level-await-7.12.1.tgz";
        sha1 = "dd6c0b357ac1bb142d98537450a319625d13d2a0";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_arrow_functions___plugin_transform_arrow_functions_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_arrow_functions___plugin_transform_arrow_functions_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-arrow-functions/-/plugin-transform-arrow-functions-7.12.1.tgz";
        sha1 = "8083ffc86ac8e777fbe24b5967c4b2521f3cb2b3";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_async_to_generator___plugin_transform_async_to_generator_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_async_to_generator___plugin_transform_async_to_generator_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-async-to-generator/-/plugin-transform-async-to-generator-7.12.1.tgz";
        sha1 = "3849a49cc2a22e9743cbd6b52926d30337229af1";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_block_scoped_functions___plugin_transform_block_scoped_functions_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_block_scoped_functions___plugin_transform_block_scoped_functions_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-block-scoped-functions/-/plugin-transform-block-scoped-functions-7.12.1.tgz";
        sha1 = "f2a1a365bde2b7112e0a6ded9067fdd7c07905d9";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_block_scoping___plugin_transform_block_scoping_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_block_scoping___plugin_transform_block_scoping_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-block-scoping/-/plugin-transform-block-scoping-7.12.1.tgz";
        sha1 = "f0ee727874b42a208a48a586b84c3d222c2bbef1";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_classes___plugin_transform_classes_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_classes___plugin_transform_classes_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-classes/-/plugin-transform-classes-7.12.1.tgz";
        sha1 = "65e650fcaddd3d88ddce67c0f834a3d436a32db6";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_computed_properties___plugin_transform_computed_properties_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_computed_properties___plugin_transform_computed_properties_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-computed-properties/-/plugin-transform-computed-properties-7.12.1.tgz";
        sha1 = "d68cf6c9b7f838a8a4144badbe97541ea0904852";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_destructuring___plugin_transform_destructuring_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_destructuring___plugin_transform_destructuring_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-destructuring/-/plugin-transform-destructuring-7.12.1.tgz";
        sha1 = "b9a570fe0d0a8d460116413cb4f97e8e08b2f847";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_dotall_regex___plugin_transform_dotall_regex_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_dotall_regex___plugin_transform_dotall_regex_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-dotall-regex/-/plugin-transform-dotall-regex-7.12.1.tgz";
        sha1 = "a1d16c14862817b6409c0a678d6f9373ca9cd975";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_duplicate_keys___plugin_transform_duplicate_keys_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_duplicate_keys___plugin_transform_duplicate_keys_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-duplicate-keys/-/plugin-transform-duplicate-keys-7.12.1.tgz";
        sha1 = "745661baba295ac06e686822797a69fbaa2ca228";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_exponentiation_operator___plugin_transform_exponentiation_operator_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_exponentiation_operator___plugin_transform_exponentiation_operator_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-exponentiation-operator/-/plugin-transform-exponentiation-operator-7.12.1.tgz";
        sha1 = "b0f2ed356ba1be1428ecaf128ff8a24f02830ae0";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_flow_strip_types___plugin_transform_flow_strip_types_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_flow_strip_types___plugin_transform_flow_strip_types_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-flow-strip-types/-/plugin-transform-flow-strip-types-7.12.1.tgz";
        sha1 = "8430decfa7eb2aea5414ed4a3fa6e1652b7d77c4";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_for_of___plugin_transform_for_of_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_for_of___plugin_transform_for_of_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-for-of/-/plugin-transform-for-of-7.12.1.tgz";
        sha1 = "07640f28867ed16f9511c99c888291f560921cfa";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_function_name___plugin_transform_function_name_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_function_name___plugin_transform_function_name_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-function-name/-/plugin-transform-function-name-7.12.1.tgz";
        sha1 = "2ec76258c70fe08c6d7da154003a480620eba667";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_literals___plugin_transform_literals_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_literals___plugin_transform_literals_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-literals/-/plugin-transform-literals-7.12.1.tgz";
        sha1 = "d73b803a26b37017ddf9d3bb8f4dc58bfb806f57";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_member_expression_literals___plugin_transform_member_expression_literals_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_member_expression_literals___plugin_transform_member_expression_literals_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-member-expression-literals/-/plugin-transform-member-expression-literals-7.12.1.tgz";
        sha1 = "496038602daf1514a64d43d8e17cbb2755e0c3ad";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_modules_amd___plugin_transform_modules_amd_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_modules_amd___plugin_transform_modules_amd_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-modules-amd/-/plugin-transform-modules-amd-7.12.1.tgz";
        sha1 = "3154300b026185666eebb0c0ed7f8415fefcf6f9";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_modules_commonjs___plugin_transform_modules_commonjs_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_modules_commonjs___plugin_transform_modules_commonjs_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-modules-commonjs/-/plugin-transform-modules-commonjs-7.12.1.tgz";
        sha1 = "fa403124542636c786cf9b460a0ffbb48a86e648";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_modules_systemjs___plugin_transform_modules_systemjs_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_modules_systemjs___plugin_transform_modules_systemjs_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-modules-systemjs/-/plugin-transform-modules-systemjs-7.12.1.tgz";
        sha1 = "663fea620d593c93f214a464cd399bf6dc683086";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_modules_umd___plugin_transform_modules_umd_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_modules_umd___plugin_transform_modules_umd_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-modules-umd/-/plugin-transform-modules-umd-7.12.1.tgz";
        sha1 = "eb5a218d6b1c68f3d6217b8fa2cc82fec6547902";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_named_capturing_groups_regex___plugin_transform_named_capturing_groups_regex_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_named_capturing_groups_regex___plugin_transform_named_capturing_groups_regex_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-named-capturing-groups-regex/-/plugin-transform-named-capturing-groups-regex-7.12.1.tgz";
        sha1 = "b407f5c96be0d9f5f88467497fa82b30ac3e8753";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_new_target___plugin_transform_new_target_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_new_target___plugin_transform_new_target_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-new-target/-/plugin-transform-new-target-7.12.1.tgz";
        sha1 = "80073f02ee1bb2d365c3416490e085c95759dec0";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_object_super___plugin_transform_object_super_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_object_super___plugin_transform_object_super_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-object-super/-/plugin-transform-object-super-7.12.1.tgz";
        sha1 = "4ea08696b8d2e65841d0c7706482b048bed1066e";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_parameters___plugin_transform_parameters_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_parameters___plugin_transform_parameters_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-parameters/-/plugin-transform-parameters-7.12.1.tgz";
        sha1 = "d2e963b038771650c922eff593799c96d853255d";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_property_literals___plugin_transform_property_literals_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_property_literals___plugin_transform_property_literals_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-property-literals/-/plugin-transform-property-literals-7.12.1.tgz";
        sha1 = "41bc81200d730abb4456ab8b3fbd5537b59adecd";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_react_jsx___plugin_transform_react_jsx_7.12.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_react_jsx___plugin_transform_react_jsx_7.12.7.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-react-jsx/-/plugin-transform-react-jsx-7.12.7.tgz";
        sha1 = "8b14d45f6eccd41b7f924bcb65c021e9f0a06f7f";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_regenerator___plugin_transform_regenerator_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_regenerator___plugin_transform_regenerator_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-regenerator/-/plugin-transform-regenerator-7.12.1.tgz";
        sha1 = "5f0a28d842f6462281f06a964e88ba8d7ab49753";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_reserved_words___plugin_transform_reserved_words_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_reserved_words___plugin_transform_reserved_words_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-reserved-words/-/plugin-transform-reserved-words-7.12.1.tgz";
        sha1 = "6fdfc8cc7edcc42b36a7c12188c6787c873adcd8";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_shorthand_properties___plugin_transform_shorthand_properties_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_shorthand_properties___plugin_transform_shorthand_properties_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-shorthand-properties/-/plugin-transform-shorthand-properties-7.12.1.tgz";
        sha1 = "0bf9cac5550fce0cfdf043420f661d645fdc75e3";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_spread___plugin_transform_spread_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_spread___plugin_transform_spread_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-spread/-/plugin-transform-spread-7.12.1.tgz";
        sha1 = "527f9f311be4ec7fdc2b79bb89f7bf884b3e1e1e";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_sticky_regex___plugin_transform_sticky_regex_7.12.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_sticky_regex___plugin_transform_sticky_regex_7.12.7.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-sticky-regex/-/plugin-transform-sticky-regex-7.12.7.tgz";
        sha1 = "560224613ab23987453948ed21d0b0b193fa7fad";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_template_literals___plugin_transform_template_literals_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_template_literals___plugin_transform_template_literals_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-template-literals/-/plugin-transform-template-literals-7.12.1.tgz";
        sha1 = "b43ece6ed9a79c0c71119f576d299ef09d942843";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_typeof_symbol___plugin_transform_typeof_symbol_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_typeof_symbol___plugin_transform_typeof_symbol_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-typeof-symbol/-/plugin-transform-typeof-symbol-7.12.1.tgz";
        sha1 = "9ca6be343d42512fbc2e68236a82ae64bc7af78a";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_unicode_escapes___plugin_transform_unicode_escapes_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_unicode_escapes___plugin_transform_unicode_escapes_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-unicode-escapes/-/plugin-transform-unicode-escapes-7.12.1.tgz";
        sha1 = "5232b9f81ccb07070b7c3c36c67a1b78f1845709";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_transform_unicode_regex___plugin_transform_unicode_regex_7.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_transform_unicode_regex___plugin_transform_unicode_regex_7.12.1.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-transform-unicode-regex/-/plugin-transform-unicode-regex-7.12.1.tgz";
        sha1 = "cc9661f61390db5c65e3febaccefd5c6ac3faecb";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_preset_env___preset_env_7.12.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_preset_env___preset_env_7.12.7.tgz";
        url  = "https://registry.npmjs.org/@babel/preset-env/-/preset-env-7.12.7.tgz";
        sha1 = "54ea21dbe92caf6f10cb1a0a576adc4ebf094b55";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_preset_modules___preset_modules_0.1.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_preset_modules___preset_modules_0.1.4.tgz";
        url  = "https://registry.npmjs.org/@babel/preset-modules/-/preset-modules-0.1.4.tgz";
        sha1 = "362f2b68c662842970fdb5e254ffc8fc1c2e415e";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_runtime___runtime_7.12.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_runtime___runtime_7.12.5.tgz";
        url  = "https://registry.npmjs.org/@babel/runtime/-/runtime-7.12.5.tgz";
        sha1 = "410e7e487441e1b360c29be715d870d9b985882e";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_template___template_7.12.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_template___template_7.12.7.tgz";
        url  = "https://registry.npmjs.org/@babel/template/-/template-7.12.7.tgz";
        sha1 = "c817233696018e39fbb6c491d2fb684e05ed43bc";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_traverse___traverse_7.12.9.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_traverse___traverse_7.12.9.tgz";
        url  = "https://registry.npmjs.org/@babel/traverse/-/traverse-7.12.9.tgz";
        sha1 = "fad26c972eabbc11350e0b695978de6cc8e8596f";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_types___types_7.12.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_types___types_7.12.7.tgz";
        url  = "https://registry.npmjs.org/@babel/types/-/types-7.12.7.tgz";
        sha1 = "6039ff1e242640a29452c9ae572162ec9a8f5d13";
      };
    }
    {
      name = "https___registry.npmjs.org__bcoe_v8_coverage___v8_coverage_0.2.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__bcoe_v8_coverage___v8_coverage_0.2.3.tgz";
        url  = "https://registry.npmjs.org/@bcoe/v8-coverage/-/v8-coverage-0.2.3.tgz";
        sha1 = "75a2e8b51cb758a7553d6804a5932d7aace75c39";
      };
    }
    {
      name = "https___registry.npmjs.org__cnakazawa_watch___watch_1.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__cnakazawa_watch___watch_1.0.4.tgz";
        url  = "https://registry.npmjs.org/@cnakazawa/watch/-/watch-1.0.4.tgz";
        sha1 = "f864ae85004d0fcab6f50be9141c4da368d1656a";
      };
    }
    {
      name = "https___registry.npmjs.org__iarna_toml___toml_2.2.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__iarna_toml___toml_2.2.5.tgz";
        url  = "https://registry.npmjs.org/@iarna/toml/-/toml-2.2.5.tgz";
        sha1 = "b32366c89b43c6f8cefbdefac778b9c828e3ba8c";
      };
    }
    {
      name = "https___registry.npmjs.org__istanbuljs_load_nyc_config___load_nyc_config_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__istanbuljs_load_nyc_config___load_nyc_config_1.1.0.tgz";
        url  = "https://registry.npmjs.org/@istanbuljs/load-nyc-config/-/load-nyc-config-1.1.0.tgz";
        sha1 = "fd3db1d59ecf7cf121e80650bb86712f9b55eced";
      };
    }
    {
      name = "https___registry.npmjs.org__istanbuljs_schema___schema_0.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__istanbuljs_schema___schema_0.1.2.tgz";
        url  = "https://registry.npmjs.org/@istanbuljs/schema/-/schema-0.1.2.tgz";
        sha1 = "26520bf09abe4a5644cd5414e37125a8954241dd";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_console___console_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_console___console_25.5.0.tgz";
        url  = "https://registry.npmjs.org/@jest/console/-/console-25.5.0.tgz";
        sha1 = "770800799d510f37329c508a9edd0b7b447d9abb";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_core___core_25.5.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_core___core_25.5.4.tgz";
        url  = "https://registry.npmjs.org/@jest/core/-/core-25.5.4.tgz";
        sha1 = "3ef7412f7339210f003cdf36646bbca786efe7b4";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_environment___environment_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_environment___environment_25.5.0.tgz";
        url  = "https://registry.npmjs.org/@jest/environment/-/environment-25.5.0.tgz";
        sha1 = "aa33b0c21a716c65686638e7ef816c0e3a0c7b37";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_fake_timers___fake_timers_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_fake_timers___fake_timers_25.5.0.tgz";
        url  = "https://registry.npmjs.org/@jest/fake-timers/-/fake-timers-25.5.0.tgz";
        sha1 = "46352e00533c024c90c2bc2ad9f2959f7f114185";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_globals___globals_25.5.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_globals___globals_25.5.2.tgz";
        url  = "https://registry.npmjs.org/@jest/globals/-/globals-25.5.2.tgz";
        sha1 = "5e45e9de8d228716af3257eeb3991cc2e162ca88";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_reporters___reporters_25.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_reporters___reporters_25.5.1.tgz";
        url  = "https://registry.npmjs.org/@jest/reporters/-/reporters-25.5.1.tgz";
        sha1 = "cb686bcc680f664c2dbaf7ed873e93aa6811538b";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_source_map___source_map_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_source_map___source_map_25.5.0.tgz";
        url  = "https://registry.npmjs.org/@jest/source-map/-/source-map-25.5.0.tgz";
        sha1 = "df5c20d6050aa292c2c6d3f0d2c7606af315bd1b";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_test_result___test_result_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_test_result___test_result_25.5.0.tgz";
        url  = "https://registry.npmjs.org/@jest/test-result/-/test-result-25.5.0.tgz";
        sha1 = "139a043230cdeffe9ba2d8341b27f2efc77ce87c";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_test_sequencer___test_sequencer_25.5.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_test_sequencer___test_sequencer_25.5.4.tgz";
        url  = "https://registry.npmjs.org/@jest/test-sequencer/-/test-sequencer-25.5.4.tgz";
        sha1 = "9b4e685b36954c38d0f052e596d28161bdc8b737";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_transform___transform_25.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_transform___transform_25.5.1.tgz";
        url  = "https://registry.npmjs.org/@jest/transform/-/transform-25.5.1.tgz";
        sha1 = "0469ddc17699dd2bf985db55fa0fb9309f5c2db3";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_types___types_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_types___types_25.5.0.tgz";
        url  = "https://registry.npmjs.org/@jest/types/-/types-25.5.0.tgz";
        sha1 = "4d6a4793f7b9599fc3680877b856a97dbccf2a9d";
      };
    }
    {
      name = "https___registry.npmjs.org__mrmlnc_readdir_enhanced___readdir_enhanced_2.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__mrmlnc_readdir_enhanced___readdir_enhanced_2.2.1.tgz";
        url  = "https://registry.npmjs.org/@mrmlnc/readdir-enhanced/-/readdir-enhanced-2.2.1.tgz";
        sha1 = "524af240d1a360527b730475ecfa1344aa540dde";
      };
    }
    {
      name = "https___registry.npmjs.org__nodelib_fs.stat___fs.stat_1.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__nodelib_fs.stat___fs.stat_1.1.3.tgz";
        url  = "https://registry.npmjs.org/@nodelib/fs.stat/-/fs.stat-1.1.3.tgz";
        sha1 = "2b5a3ab3f918cca48a8c754c08168e3f03eba61b";
      };
    }
    {
      name = "https___registry.npmjs.org__parcel_fs___fs_1.11.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__parcel_fs___fs_1.11.0.tgz";
        url  = "https://registry.npmjs.org/@parcel/fs/-/fs-1.11.0.tgz";
        sha1 = "fb8a2be038c454ad46a50dc0554c1805f13535cd";
      };
    }
    {
      name = "https___registry.npmjs.org__parcel_logger___logger_1.11.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__parcel_logger___logger_1.11.1.tgz";
        url  = "https://registry.npmjs.org/@parcel/logger/-/logger-1.11.1.tgz";
        sha1 = "c55b0744bcbe84ebc291155627f0ec406a23e2e6";
      };
    }
    {
      name = "https___registry.npmjs.org__parcel_utils___utils_1.11.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__parcel_utils___utils_1.11.0.tgz";
        url  = "https://registry.npmjs.org/@parcel/utils/-/utils-1.11.0.tgz";
        sha1 = "539e08fff8af3b26eca11302be80b522674b51ea";
      };
    }
    {
      name = "https___registry.npmjs.org__parcel_watcher___watcher_1.12.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__parcel_watcher___watcher_1.12.1.tgz";
        url  = "https://registry.npmjs.org/@parcel/watcher/-/watcher-1.12.1.tgz";
        sha1 = "b98b3df309fcab93451b5583fc38e40826696dad";
      };
    }
    {
      name = "https___registry.npmjs.org__parcel_workers___workers_1.11.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__parcel_workers___workers_1.11.0.tgz";
        url  = "https://registry.npmjs.org/@parcel/workers/-/workers-1.11.0.tgz";
        sha1 = "7b8dcf992806f4ad2b6cecf629839c41c2336c59";
      };
    }
    {
      name = "https___registry.npmjs.org__sinonjs_commons___commons_1.8.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__sinonjs_commons___commons_1.8.1.tgz";
        url  = "https://registry.npmjs.org/@sinonjs/commons/-/commons-1.8.1.tgz";
        sha1 = "e7df00f98a203324f6dc7cc606cad9d4a8ab2217";
      };
    }
    {
      name = "https___registry.npmjs.org__types_babel__core___babel__core_7.1.12.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_babel__core___babel__core_7.1.12.tgz";
        url  = "https://registry.npmjs.org/@types/babel__core/-/babel__core-7.1.12.tgz";
        sha1 = "4d8e9e51eb265552a7e4f1ff2219ab6133bdfb2d";
      };
    }
    {
      name = "https___registry.npmjs.org__types_babel__generator___babel__generator_7.6.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_babel__generator___babel__generator_7.6.2.tgz";
        url  = "https://registry.npmjs.org/@types/babel__generator/-/babel__generator-7.6.2.tgz";
        sha1 = "f3d71178e187858f7c45e30380f8f1b7415a12d8";
      };
    }
    {
      name = "https___registry.npmjs.org__types_babel__template___babel__template_7.4.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_babel__template___babel__template_7.4.0.tgz";
        url  = "https://registry.npmjs.org/@types/babel__template/-/babel__template-7.4.0.tgz";
        sha1 = "0c888dd70b3ee9eebb6e4f200e809da0076262be";
      };
    }
    {
      name = "https___registry.npmjs.org__types_babel__traverse___babel__traverse_7.11.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_babel__traverse___babel__traverse_7.11.0.tgz";
        url  = "https://registry.npmjs.org/@types/babel__traverse/-/babel__traverse-7.11.0.tgz";
        sha1 = "b9a1efa635201ba9bc850323a8793ee2d36c04a0";
      };
    }
    {
      name = "https___registry.npmjs.org__types_graceful_fs___graceful_fs_4.1.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_graceful_fs___graceful_fs_4.1.4.tgz";
        url  = "https://registry.npmjs.org/@types/graceful-fs/-/graceful-fs-4.1.4.tgz";
        sha1 = "4ff9f641a7c6d1a3508ff88bc3141b152772e753";
      };
    }
    {
      name = "https___registry.npmjs.org__types_istanbul_lib_coverage___istanbul_lib_coverage_2.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_istanbul_lib_coverage___istanbul_lib_coverage_2.0.3.tgz";
        url  = "https://registry.npmjs.org/@types/istanbul-lib-coverage/-/istanbul-lib-coverage-2.0.3.tgz";
        sha1 = "4ba8ddb720221f432e443bd5f9117fd22cfd4762";
      };
    }
    {
      name = "https___registry.npmjs.org__types_istanbul_lib_report___istanbul_lib_report_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_istanbul_lib_report___istanbul_lib_report_3.0.0.tgz";
        url  = "https://registry.npmjs.org/@types/istanbul-lib-report/-/istanbul-lib-report-3.0.0.tgz";
        sha1 = "c14c24f18ea8190c118ee7562b7ff99a36552686";
      };
    }
    {
      name = "https___registry.npmjs.org__types_istanbul_reports___istanbul_reports_1.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_istanbul_reports___istanbul_reports_1.1.2.tgz";
        url  = "https://registry.npmjs.org/@types/istanbul-reports/-/istanbul-reports-1.1.2.tgz";
        sha1 = "e875cc689e47bce549ec81f3df5e6f6f11cfaeb2";
      };
    }
    {
      name = "https___registry.npmjs.org__types_jest___jest_25.2.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_jest___jest_25.2.3.tgz";
        url  = "https://registry.npmjs.org/@types/jest/-/jest-25.2.3.tgz";
        sha1 = "33d27e4c4716caae4eced355097a47ad363fdcaf";
      };
    }
    {
      name = "https___registry.npmjs.org__types_node___node_14.14.11.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_node___node_14.14.11.tgz";
        url  = "https://registry.npmjs.org/@types/node/-/node-14.14.11.tgz";
        sha1 = "fc25a4248a5e8d0837019b1d170146d07334abe0";
      };
    }
    {
      name = "https___registry.npmjs.org__types_normalize_package_data___normalize_package_data_2.4.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_normalize_package_data___normalize_package_data_2.4.0.tgz";
        url  = "https://registry.npmjs.org/@types/normalize-package-data/-/normalize-package-data-2.4.0.tgz";
        sha1 = "e486d0d97396d79beedd0a6e33f4534ff6b4973e";
      };
    }
    {
      name = "https___registry.npmjs.org__types_prettier___prettier_1.19.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_prettier___prettier_1.19.1.tgz";
        url  = "https://registry.npmjs.org/@types/prettier/-/prettier-1.19.1.tgz";
        sha1 = "33509849f8e679e4add158959fdb086440e9553f";
      };
    }
    {
      name = "https___registry.npmjs.org__types_q___q_1.5.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_q___q_1.5.4.tgz";
        url  = "https://registry.npmjs.org/@types/q/-/q-1.5.4.tgz";
        sha1 = "15925414e0ad2cd765bfef58842f7e26a7accb24";
      };
    }
    {
      name = "https___registry.npmjs.org__types_stack_utils___stack_utils_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_stack_utils___stack_utils_1.0.1.tgz";
        url  = "https://registry.npmjs.org/@types/stack-utils/-/stack-utils-1.0.1.tgz";
        sha1 = "0a851d3bd96498fa25c33ab7278ed3bd65f06c3e";
      };
    }
    {
      name = "https___registry.npmjs.org__types_yargs_parser___yargs_parser_15.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_yargs_parser___yargs_parser_15.0.0.tgz";
        url  = "https://registry.npmjs.org/@types/yargs-parser/-/yargs-parser-15.0.0.tgz";
        sha1 = "cb3f9f741869e20cce330ffbeb9271590483882d";
      };
    }
    {
      name = "https___registry.npmjs.org__types_yargs___yargs_15.0.11.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_yargs___yargs_15.0.11.tgz";
        url  = "https://registry.npmjs.org/@types/yargs/-/yargs-15.0.11.tgz";
        sha1 = "361d7579ecdac1527687bcebf9946621c12ab78c";
      };
    }
    {
      name = "https___registry.npmjs.org_abab___abab_2.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_abab___abab_2.0.5.tgz";
        url  = "https://registry.npmjs.org/abab/-/abab-2.0.5.tgz";
        sha1 = "c0b678fb32d60fc1219c784d6a826fe385aeb79a";
      };
    }
    {
      name = "https___registry.npmjs.org_acorn_globals___acorn_globals_4.3.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_acorn_globals___acorn_globals_4.3.4.tgz";
        url  = "https://registry.npmjs.org/acorn-globals/-/acorn-globals-4.3.4.tgz";
        sha1 = "9fa1926addc11c97308c4e66d7add0d40c3272e7";
      };
    }
    {
      name = "https___registry.npmjs.org_acorn_walk___acorn_walk_6.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_acorn_walk___acorn_walk_6.2.0.tgz";
        url  = "https://registry.npmjs.org/acorn-walk/-/acorn-walk-6.2.0.tgz";
        sha1 = "123cb8f3b84c2171f1f7fb252615b1c78a6b1a8c";
      };
    }
    {
      name = "https___registry.npmjs.org_acorn___acorn_6.4.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_acorn___acorn_6.4.2.tgz";
        url  = "https://registry.npmjs.org/acorn/-/acorn-6.4.2.tgz";
        sha1 = "35866fd710528e92de10cf06016498e47e39e1e6";
      };
    }
    {
      name = "https___registry.npmjs.org_acorn___acorn_7.4.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_acorn___acorn_7.4.1.tgz";
        url  = "https://registry.npmjs.org/acorn/-/acorn-7.4.1.tgz";
        sha1 = "feaed255973d2e77555b83dbc08851a6c63520fa";
      };
    }
    {
      name = "https___registry.npmjs.org_ajv___ajv_6.12.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ajv___ajv_6.12.6.tgz";
        url  = "https://registry.npmjs.org/ajv/-/ajv-6.12.6.tgz";
        sha1 = "baf5a62e802b07d977034586f8c3baf5adf26df4";
      };
    }
    {
      name = "https___registry.npmjs.org_alphanum_sort___alphanum_sort_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_alphanum_sort___alphanum_sort_1.0.2.tgz";
        url  = "https://registry.npmjs.org/alphanum-sort/-/alphanum-sort-1.0.2.tgz";
        sha1 = "97a1119649b211ad33691d9f9f486a8ec9fbe0a3";
      };
    }
    {
      name = "https___registry.npmjs.org_ansi_escapes___ansi_escapes_4.3.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ansi_escapes___ansi_escapes_4.3.1.tgz";
        url  = "https://registry.npmjs.org/ansi-escapes/-/ansi-escapes-4.3.1.tgz";
        sha1 = "a5c47cc43181f1f38ffd7076837700d395522a61";
      };
    }
    {
      name = "https___registry.npmjs.org_ansi_regex___ansi_regex_2.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ansi_regex___ansi_regex_2.1.1.tgz";
        url  = "https://registry.npmjs.org/ansi-regex/-/ansi-regex-2.1.1.tgz";
        sha1 = "c3b33ab5ee360d86e0e628f0468ae7ef27d654df";
      };
    }
    {
      name = "https___registry.npmjs.org_ansi_regex___ansi_regex_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ansi_regex___ansi_regex_3.0.0.tgz";
        url  = "https://registry.npmjs.org/ansi-regex/-/ansi-regex-3.0.0.tgz";
        sha1 = "ed0317c322064f79466c02966bddb605ab37d998";
      };
    }
    {
      name = "https___registry.npmjs.org_ansi_regex___ansi_regex_4.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ansi_regex___ansi_regex_4.1.0.tgz";
        url  = "https://registry.npmjs.org/ansi-regex/-/ansi-regex-4.1.0.tgz";
        sha1 = "8b9f8f08cf1acb843756a839ca8c7e3168c51997";
      };
    }
    {
      name = "https___registry.npmjs.org_ansi_regex___ansi_regex_5.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ansi_regex___ansi_regex_5.0.0.tgz";
        url  = "https://registry.npmjs.org/ansi-regex/-/ansi-regex-5.0.0.tgz";
        sha1 = "388539f55179bf39339c81af30a654d69f87cb75";
      };
    }
    {
      name = "https___registry.npmjs.org_ansi_styles___ansi_styles_2.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ansi_styles___ansi_styles_2.2.1.tgz";
        url  = "https://registry.npmjs.org/ansi-styles/-/ansi-styles-2.2.1.tgz";
        sha1 = "b432dd3358b634cf75e1e4664368240533c1ddbe";
      };
    }
    {
      name = "https___registry.npmjs.org_ansi_styles___ansi_styles_3.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ansi_styles___ansi_styles_3.2.1.tgz";
        url  = "https://registry.npmjs.org/ansi-styles/-/ansi-styles-3.2.1.tgz";
        sha1 = "41fbb20243e50b12be0f04b8dedbf07520ce841d";
      };
    }
    {
      name = "https___registry.npmjs.org_ansi_styles___ansi_styles_4.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ansi_styles___ansi_styles_4.3.0.tgz";
        url  = "https://registry.npmjs.org/ansi-styles/-/ansi-styles-4.3.0.tgz";
        sha1 = "edd803628ae71c04c85ae7a0906edad34b648937";
      };
    }
    {
      name = "https___registry.npmjs.org_ansi_to_html___ansi_to_html_0.6.14.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ansi_to_html___ansi_to_html_0.6.14.tgz";
        url  = "https://registry.npmjs.org/ansi-to-html/-/ansi-to-html-0.6.14.tgz";
        sha1 = "65fe6d08bba5dd9db33f44a20aec331e0010dad8";
      };
    }
    {
      name = "https___registry.npmjs.org_anymatch___anymatch_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_anymatch___anymatch_2.0.0.tgz";
        url  = "https://registry.npmjs.org/anymatch/-/anymatch-2.0.0.tgz";
        sha1 = "bcb24b4f37934d9aa7ac17b4adaf89e7c76ef2eb";
      };
    }
    {
      name = "https___registry.npmjs.org_anymatch___anymatch_3.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_anymatch___anymatch_3.1.1.tgz";
        url  = "https://registry.npmjs.org/anymatch/-/anymatch-3.1.1.tgz";
        sha1 = "c55ecf02185e2469259399310c173ce31233b142";
      };
    }
    {
      name = "https___registry.npmjs.org_argparse___argparse_1.0.10.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_argparse___argparse_1.0.10.tgz";
        url  = "https://registry.npmjs.org/argparse/-/argparse-1.0.10.tgz";
        sha1 = "bcd6791ea5ae09725e17e5ad988134cd40b3d911";
      };
    }
    {
      name = "https___registry.npmjs.org_arr_diff___arr_diff_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_arr_diff___arr_diff_4.0.0.tgz";
        url  = "https://registry.npmjs.org/arr-diff/-/arr-diff-4.0.0.tgz";
        sha1 = "d6461074febfec71e7e15235761a329a5dc7c520";
      };
    }
    {
      name = "https___registry.npmjs.org_arr_flatten___arr_flatten_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_arr_flatten___arr_flatten_1.1.0.tgz";
        url  = "https://registry.npmjs.org/arr-flatten/-/arr-flatten-1.1.0.tgz";
        sha1 = "36048bbff4e7b47e136644316c99669ea5ae91f1";
      };
    }
    {
      name = "https___registry.npmjs.org_arr_union___arr_union_3.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_arr_union___arr_union_3.1.0.tgz";
        url  = "https://registry.npmjs.org/arr-union/-/arr-union-3.1.0.tgz";
        sha1 = "e39b09aea9def866a8f206e288af63919bae39c4";
      };
    }
    {
      name = "https___registry.npmjs.org_array_equal___array_equal_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_array_equal___array_equal_1.0.0.tgz";
        url  = "https://registry.npmjs.org/array-equal/-/array-equal-1.0.0.tgz";
        sha1 = "8c2a5ef2472fd9ea742b04c77a75093ba2757c93";
      };
    }
    {
      name = "https___registry.npmjs.org_array_unique___array_unique_0.3.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_array_unique___array_unique_0.3.2.tgz";
        url  = "https://registry.npmjs.org/array-unique/-/array-unique-0.3.2.tgz";
        sha1 = "a894b75d4bc4f6cd679ef3244a9fd8f46ae2d428";
      };
    }
    {
      name = "https___registry.npmjs.org_asn1.js___asn1.js_5.4.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_asn1.js___asn1.js_5.4.1.tgz";
        url  = "https://registry.npmjs.org/asn1.js/-/asn1.js-5.4.1.tgz";
        sha1 = "11a980b84ebb91781ce35b0fdc2ee294e3783f07";
      };
    }
    {
      name = "https___registry.npmjs.org_asn1___asn1_0.2.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_asn1___asn1_0.2.4.tgz";
        url  = "https://registry.npmjs.org/asn1/-/asn1-0.2.4.tgz";
        sha1 = "8d2475dfab553bb33e77b54e59e880bb8ce23136";
      };
    }
    {
      name = "https___registry.npmjs.org_assert_plus___assert_plus_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_assert_plus___assert_plus_1.0.0.tgz";
        url  = "https://registry.npmjs.org/assert-plus/-/assert-plus-1.0.0.tgz";
        sha1 = "f12e0f3c5d77b0b1cdd9146942e4e96c1e4dd525";
      };
    }
    {
      name = "https___registry.npmjs.org_assert___assert_1.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_assert___assert_1.5.0.tgz";
        url  = "https://registry.npmjs.org/assert/-/assert-1.5.0.tgz";
        sha1 = "55c109aaf6e0aefdb3dc4b71240c70bf574b18eb";
      };
    }
    {
      name = "https___registry.npmjs.org_assign_symbols___assign_symbols_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_assign_symbols___assign_symbols_1.0.0.tgz";
        url  = "https://registry.npmjs.org/assign-symbols/-/assign-symbols-1.0.0.tgz";
        sha1 = "59667f41fadd4f20ccbc2bb96b8d4f7f78ec0367";
      };
    }
    {
      name = "https___registry.npmjs.org_astral_regex___astral_regex_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_astral_regex___astral_regex_1.0.0.tgz";
        url  = "https://registry.npmjs.org/astral-regex/-/astral-regex-1.0.0.tgz";
        sha1 = "6c8c3fb827dd43ee3918f27b82782ab7658a6fd9";
      };
    }
    {
      name = "https___registry.npmjs.org_async_each___async_each_1.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_async_each___async_each_1.0.3.tgz";
        url  = "https://registry.npmjs.org/async-each/-/async-each-1.0.3.tgz";
        sha1 = "b727dbf87d7651602f06f4d4ac387f47d91b0cbf";
      };
    }
    {
      name = "https___registry.npmjs.org_async_limiter___async_limiter_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_async_limiter___async_limiter_1.0.1.tgz";
        url  = "https://registry.npmjs.org/async-limiter/-/async-limiter-1.0.1.tgz";
        sha1 = "dd379e94f0db8310b08291f9d64c3209766617fd";
      };
    }
    {
      name = "https___registry.npmjs.org_asynckit___asynckit_0.4.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_asynckit___asynckit_0.4.0.tgz";
        url  = "https://registry.npmjs.org/asynckit/-/asynckit-0.4.0.tgz";
        sha1 = "c79ed97f7f34cb8f2ba1bc9790bcc366474b4b79";
      };
    }
    {
      name = "https___registry.npmjs.org_atob___atob_2.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_atob___atob_2.1.2.tgz";
        url  = "https://registry.npmjs.org/atob/-/atob-2.1.2.tgz";
        sha1 = "6d9517eb9e030d2436666651e86bd9f6f13533c9";
      };
    }
    {
      name = "https___registry.npmjs.org_aws_sign2___aws_sign2_0.7.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_aws_sign2___aws_sign2_0.7.0.tgz";
        url  = "https://registry.npmjs.org/aws-sign2/-/aws-sign2-0.7.0.tgz";
        sha1 = "b46e890934a9591f2d2f6f86d7e6a9f1b3fe76a8";
      };
    }
    {
      name = "https___registry.npmjs.org_aws4___aws4_1.11.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_aws4___aws4_1.11.0.tgz";
        url  = "https://registry.npmjs.org/aws4/-/aws4-1.11.0.tgz";
        sha1 = "d61f46d83b2519250e2784daf5b09479a8b41c59";
      };
    }
    {
      name = "https___registry.npmjs.org_babel_jest___babel_jest_25.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_babel_jest___babel_jest_25.5.1.tgz";
        url  = "https://registry.npmjs.org/babel-jest/-/babel-jest-25.5.1.tgz";
        sha1 = "bc2e6101f849d6f6aec09720ffc7bc5332e62853";
      };
    }
    {
      name = "https___registry.npmjs.org_babel_plugin_dynamic_import_node___babel_plugin_dynamic_import_node_2.3.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_babel_plugin_dynamic_import_node___babel_plugin_dynamic_import_node_2.3.3.tgz";
        url  = "https://registry.npmjs.org/babel-plugin-dynamic-import-node/-/babel-plugin-dynamic-import-node-2.3.3.tgz";
        sha1 = "84fda19c976ec5c6defef57f9427b3def66e17a3";
      };
    }
    {
      name = "https___registry.npmjs.org_babel_plugin_istanbul___babel_plugin_istanbul_6.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_babel_plugin_istanbul___babel_plugin_istanbul_6.0.0.tgz";
        url  = "https://registry.npmjs.org/babel-plugin-istanbul/-/babel-plugin-istanbul-6.0.0.tgz";
        sha1 = "e159ccdc9af95e0b570c75b4573b7c34d671d765";
      };
    }
    {
      name = "https___registry.npmjs.org_babel_plugin_jest_hoist___babel_plugin_jest_hoist_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_babel_plugin_jest_hoist___babel_plugin_jest_hoist_25.5.0.tgz";
        url  = "https://registry.npmjs.org/babel-plugin-jest-hoist/-/babel-plugin-jest-hoist-25.5.0.tgz";
        sha1 = "129c80ba5c7fc75baf3a45b93e2e372d57ca2677";
      };
    }
    {
      name = "https___registry.npmjs.org_babel_preset_current_node_syntax___babel_preset_current_node_syntax_0.1.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_babel_preset_current_node_syntax___babel_preset_current_node_syntax_0.1.4.tgz";
        url  = "https://registry.npmjs.org/babel-preset-current-node-syntax/-/babel-preset-current-node-syntax-0.1.4.tgz";
        sha1 = "826f1f8e7245ad534714ba001f84f7e906c3b615";
      };
    }
    {
      name = "https___registry.npmjs.org_babel_preset_jest___babel_preset_jest_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_babel_preset_jest___babel_preset_jest_25.5.0.tgz";
        url  = "https://registry.npmjs.org/babel-preset-jest/-/babel-preset-jest-25.5.0.tgz";
        sha1 = "c1d7f191829487a907764c65307faa0e66590b49";
      };
    }
    {
      name = "https___registry.npmjs.org_babel_runtime___babel_runtime_6.26.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_babel_runtime___babel_runtime_6.26.0.tgz";
        url  = "https://registry.npmjs.org/babel-runtime/-/babel-runtime-6.26.0.tgz";
        sha1 = "965c7058668e82b55d7bfe04ff2337bc8b5647fe";
      };
    }
    {
      name = "https___registry.npmjs.org_babel_types___babel_types_6.26.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_babel_types___babel_types_6.26.0.tgz";
        url  = "https://registry.npmjs.org/babel-types/-/babel-types-6.26.0.tgz";
        sha1 = "a3b073f94ab49eb6fa55cd65227a334380632497";
      };
    }
    {
      name = "https___registry.npmjs.org_babylon_walk___babylon_walk_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_babylon_walk___babylon_walk_1.0.2.tgz";
        url  = "https://registry.npmjs.org/babylon-walk/-/babylon-walk-1.0.2.tgz";
        sha1 = "3b15a5ddbb482a78b4ce9c01c8ba181702d9d6ce";
      };
    }
    {
      name = "https___registry.npmjs.org_balanced_match___balanced_match_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_balanced_match___balanced_match_1.0.0.tgz";
        url  = "https://registry.npmjs.org/balanced-match/-/balanced-match-1.0.0.tgz";
        sha1 = "89b4d199ab2bee49de164ea02b89ce462d71b767";
      };
    }
    {
      name = "https___registry.npmjs.org_base64_js___base64_js_1.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_base64_js___base64_js_1.5.1.tgz";
        url  = "https://registry.npmjs.org/base64-js/-/base64-js-1.5.1.tgz";
        sha1 = "1b1b440160a5bf7ad40b650f095963481903930a";
      };
    }
    {
      name = "https___registry.npmjs.org_base___base_0.11.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_base___base_0.11.2.tgz";
        url  = "https://registry.npmjs.org/base/-/base-0.11.2.tgz";
        sha1 = "7bde5ced145b6d551a90db87f83c558b4eb48a8f";
      };
    }
    {
      name = "https___registry.npmjs.org_bcrypt_pbkdf___bcrypt_pbkdf_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_bcrypt_pbkdf___bcrypt_pbkdf_1.0.2.tgz";
        url  = "https://registry.npmjs.org/bcrypt-pbkdf/-/bcrypt-pbkdf-1.0.2.tgz";
        sha1 = "a4301d389b6a43f9b67ff3ca11a3f6637e360e9e";
      };
    }
    {
      name = "https___registry.npmjs.org_binary_extensions___binary_extensions_1.13.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_binary_extensions___binary_extensions_1.13.1.tgz";
        url  = "https://registry.npmjs.org/binary-extensions/-/binary-extensions-1.13.1.tgz";
        sha1 = "598afe54755b2868a5330d2aff9d4ebb53209b65";
      };
    }
    {
      name = "https___registry.npmjs.org_bindings___bindings_1.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_bindings___bindings_1.5.0.tgz";
        url  = "https://registry.npmjs.org/bindings/-/bindings-1.5.0.tgz";
        sha1 = "10353c9e945334bc0511a6d90b38fbc7c9c504df";
      };
    }
    {
      name = "https___registry.npmjs.org_bn.js___bn.js_4.11.9.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_bn.js___bn.js_4.11.9.tgz";
        url  = "https://registry.npmjs.org/bn.js/-/bn.js-4.11.9.tgz";
        sha1 = "26d556829458f9d1e81fc48952493d0ba3507828";
      };
    }
    {
      name = "https___registry.npmjs.org_bn.js___bn.js_5.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_bn.js___bn.js_5.1.3.tgz";
        url  = "https://registry.npmjs.org/bn.js/-/bn.js-5.1.3.tgz";
        sha1 = "beca005408f642ebebea80b042b4d18d2ac0ee6b";
      };
    }
    {
      name = "https___registry.npmjs.org_boolbase___boolbase_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_boolbase___boolbase_1.0.0.tgz";
        url  = "https://registry.npmjs.org/boolbase/-/boolbase-1.0.0.tgz";
        sha1 = "68dff5fbe60c51eb37725ea9e3ed310dcc1e776e";
      };
    }
    {
      name = "https___registry.npmjs.org_brace_expansion___brace_expansion_1.1.11.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_brace_expansion___brace_expansion_1.1.11.tgz";
        url  = "https://registry.npmjs.org/brace-expansion/-/brace-expansion-1.1.11.tgz";
        sha1 = "3c7fcbf529d87226f3d2f52b966ff5271eb441dd";
      };
    }
    {
      name = "https___registry.npmjs.org_braces___braces_2.3.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_braces___braces_2.3.2.tgz";
        url  = "https://registry.npmjs.org/braces/-/braces-2.3.2.tgz";
        sha1 = "5979fd3f14cd531565e5fa2df1abfff1dfaee729";
      };
    }
    {
      name = "https___registry.npmjs.org_braces___braces_3.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_braces___braces_3.0.2.tgz";
        url  = "https://registry.npmjs.org/braces/-/braces-3.0.2.tgz";
        sha1 = "3454e1a462ee8d599e236df336cd9ea4f8afe107";
      };
    }
    {
      name = "https___registry.npmjs.org_brfs___brfs_1.6.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_brfs___brfs_1.6.1.tgz";
        url  = "https://registry.npmjs.org/brfs/-/brfs-1.6.1.tgz";
        sha1 = "b78ce2336d818e25eea04a0947cba6d4fb8849c3";
      };
    }
    {
      name = "https___registry.npmjs.org_brorand___brorand_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_brorand___brorand_1.1.0.tgz";
        url  = "https://registry.npmjs.org/brorand/-/brorand-1.1.0.tgz";
        sha1 = "12c25efe40a45e3c323eb8675a0a0ce57b22371f";
      };
    }
    {
      name = "https___registry.npmjs.org_browser_process_hrtime___browser_process_hrtime_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_browser_process_hrtime___browser_process_hrtime_1.0.0.tgz";
        url  = "https://registry.npmjs.org/browser-process-hrtime/-/browser-process-hrtime-1.0.0.tgz";
        sha1 = "3c9b4b7d782c8121e56f10106d84c0d0ffc94626";
      };
    }
    {
      name = "https___registry.npmjs.org_browser_resolve___browser_resolve_1.11.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_browser_resolve___browser_resolve_1.11.3.tgz";
        url  = "https://registry.npmjs.org/browser-resolve/-/browser-resolve-1.11.3.tgz";
        sha1 = "9b7cbb3d0f510e4cb86bdbd796124d28b5890af6";
      };
    }
    {
      name = "https___registry.npmjs.org_browserify_aes___browserify_aes_1.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_browserify_aes___browserify_aes_1.2.0.tgz";
        url  = "https://registry.npmjs.org/browserify-aes/-/browserify-aes-1.2.0.tgz";
        sha1 = "326734642f403dabc3003209853bb70ad428ef48";
      };
    }
    {
      name = "https___registry.npmjs.org_browserify_cipher___browserify_cipher_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_browserify_cipher___browserify_cipher_1.0.1.tgz";
        url  = "https://registry.npmjs.org/browserify-cipher/-/browserify-cipher-1.0.1.tgz";
        sha1 = "8d6474c1b870bfdabcd3bcfcc1934a10e94f15f0";
      };
    }
    {
      name = "https___registry.npmjs.org_browserify_des___browserify_des_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_browserify_des___browserify_des_1.0.2.tgz";
        url  = "https://registry.npmjs.org/browserify-des/-/browserify-des-1.0.2.tgz";
        sha1 = "3af4f1f59839403572f1c66204375f7a7f703e9c";
      };
    }
    {
      name = "https___registry.npmjs.org_browserify_rsa___browserify_rsa_4.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_browserify_rsa___browserify_rsa_4.1.0.tgz";
        url  = "https://registry.npmjs.org/browserify-rsa/-/browserify-rsa-4.1.0.tgz";
        sha1 = "b2fd06b5b75ae297f7ce2dc651f918f5be158c8d";
      };
    }
    {
      name = "https___registry.npmjs.org_browserify_sign___browserify_sign_4.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_browserify_sign___browserify_sign_4.2.1.tgz";
        url  = "https://registry.npmjs.org/browserify-sign/-/browserify-sign-4.2.1.tgz";
        sha1 = "eaf4add46dd54be3bb3b36c0cf15abbeba7956c3";
      };
    }
    {
      name = "https___registry.npmjs.org_browserify_zlib___browserify_zlib_0.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_browserify_zlib___browserify_zlib_0.2.0.tgz";
        url  = "https://registry.npmjs.org/browserify-zlib/-/browserify-zlib-0.2.0.tgz";
        sha1 = "2869459d9aa3be245fe8fe2ca1f46e2e7f54d73f";
      };
    }
    {
      name = "https___registry.npmjs.org_browserslist___browserslist_4.15.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_browserslist___browserslist_4.15.0.tgz";
        url  = "https://registry.npmjs.org/browserslist/-/browserslist-4.15.0.tgz";
        sha1 = "3d48bbca6a3f378e86102ffd017d9a03f122bdb0";
      };
    }
    {
      name = "https___registry.npmjs.org_bs_logger___bs_logger_0.2.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_bs_logger___bs_logger_0.2.6.tgz";
        url  = "https://registry.npmjs.org/bs-logger/-/bs-logger-0.2.6.tgz";
        sha1 = "eb7d365307a72cf974cc6cda76b68354ad336bd8";
      };
    }
    {
      name = "https___registry.npmjs.org_bser___bser_2.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_bser___bser_2.1.1.tgz";
        url  = "https://registry.npmjs.org/bser/-/bser-2.1.1.tgz";
        sha1 = "e6787da20ece9d07998533cfd9de6f5c38f4bc05";
      };
    }
    {
      name = "https___registry.npmjs.org_buffer_equal___buffer_equal_0.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_buffer_equal___buffer_equal_0.0.1.tgz";
        url  = "https://registry.npmjs.org/buffer-equal/-/buffer-equal-0.0.1.tgz";
        sha1 = "91bc74b11ea405bc916bc6aa908faafa5b4aac4b";
      };
    }
    {
      name = "https___registry.npmjs.org_buffer_from___buffer_from_1.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_buffer_from___buffer_from_1.1.1.tgz";
        url  = "https://registry.npmjs.org/buffer-from/-/buffer-from-1.1.1.tgz";
        sha1 = "32713bc028f75c02fdb710d7c7bcec1f2c6070ef";
      };
    }
    {
      name = "https___registry.npmjs.org_buffer_xor___buffer_xor_1.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_buffer_xor___buffer_xor_1.0.3.tgz";
        url  = "https://registry.npmjs.org/buffer-xor/-/buffer-xor-1.0.3.tgz";
        sha1 = "26e61ed1422fb70dd42e6e36729ed51d855fe8d9";
      };
    }
    {
      name = "https___registry.npmjs.org_buffer___buffer_4.9.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_buffer___buffer_4.9.2.tgz";
        url  = "https://registry.npmjs.org/buffer/-/buffer-4.9.2.tgz";
        sha1 = "230ead344002988644841ab0244af8c44bbe3ef8";
      };
    }
    {
      name = "https___registry.npmjs.org_builtin_status_codes___builtin_status_codes_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_builtin_status_codes___builtin_status_codes_3.0.0.tgz";
        url  = "https://registry.npmjs.org/builtin-status-codes/-/builtin-status-codes-3.0.0.tgz";
        sha1 = "85982878e21b98e1c66425e03d0174788f569ee8";
      };
    }
    {
      name = "https___registry.npmjs.org_cache_base___cache_base_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cache_base___cache_base_1.0.1.tgz";
        url  = "https://registry.npmjs.org/cache-base/-/cache-base-1.0.1.tgz";
        sha1 = "0a7f46416831c8b662ee36fe4e7c59d76f666ab2";
      };
    }
    {
      name = "https___registry.npmjs.org_call_bind___call_bind_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_call_bind___call_bind_1.0.0.tgz";
        url  = "https://registry.npmjs.org/call-bind/-/call-bind-1.0.0.tgz";
        sha1 = "24127054bb3f9bdcb4b1fb82418186072f77b8ce";
      };
    }
    {
      name = "https___registry.npmjs.org_call_me_maybe___call_me_maybe_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_call_me_maybe___call_me_maybe_1.0.1.tgz";
        url  = "https://registry.npmjs.org/call-me-maybe/-/call-me-maybe-1.0.1.tgz";
        sha1 = "26d208ea89e37b5cbde60250a15f031c16a4d66b";
      };
    }
    {
      name = "https___registry.npmjs.org_caller_callsite___caller_callsite_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_caller_callsite___caller_callsite_2.0.0.tgz";
        url  = "https://registry.npmjs.org/caller-callsite/-/caller-callsite-2.0.0.tgz";
        sha1 = "847e0fce0a223750a9a027c54b33731ad3154134";
      };
    }
    {
      name = "https___registry.npmjs.org_caller_path___caller_path_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_caller_path___caller_path_2.0.0.tgz";
        url  = "https://registry.npmjs.org/caller-path/-/caller-path-2.0.0.tgz";
        sha1 = "468f83044e369ab2010fac5f06ceee15bb2cb1f4";
      };
    }
    {
      name = "https___registry.npmjs.org_callsites___callsites_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_callsites___callsites_2.0.0.tgz";
        url  = "https://registry.npmjs.org/callsites/-/callsites-2.0.0.tgz";
        sha1 = "06eb84f00eea413da86affefacbffb36093b3c50";
      };
    }
    {
      name = "https___registry.npmjs.org_callsites___callsites_3.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_callsites___callsites_3.1.0.tgz";
        url  = "https://registry.npmjs.org/callsites/-/callsites-3.1.0.tgz";
        sha1 = "b3630abd8943432f54b3f0519238e33cd7df2f73";
      };
    }
    {
      name = "https___registry.npmjs.org_camelcase___camelcase_5.3.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_camelcase___camelcase_5.3.1.tgz";
        url  = "https://registry.npmjs.org/camelcase/-/camelcase-5.3.1.tgz";
        sha1 = "e3c9b31569e106811df242f715725a1f4c494320";
      };
    }
    {
      name = "https___registry.npmjs.org_caniuse_api___caniuse_api_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_caniuse_api___caniuse_api_3.0.0.tgz";
        url  = "https://registry.npmjs.org/caniuse-api/-/caniuse-api-3.0.0.tgz";
        sha1 = "5e4d90e2274961d46291997df599e3ed008ee4c0";
      };
    }
    {
      name = "https___registry.npmjs.org_caniuse_lite___caniuse_lite_1.0.30001165.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_caniuse_lite___caniuse_lite_1.0.30001165.tgz";
        url  = "https://registry.npmjs.org/caniuse-lite/-/caniuse-lite-1.0.30001165.tgz";
        sha1 = "32955490d2f60290bb186bb754f2981917fa744f";
      };
    }
    {
      name = "https___registry.npmjs.org_capture_exit___capture_exit_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_capture_exit___capture_exit_2.0.0.tgz";
        url  = "https://registry.npmjs.org/capture-exit/-/capture-exit-2.0.0.tgz";
        sha1 = "fb953bfaebeb781f62898239dabb426d08a509a4";
      };
    }
    {
      name = "https___registry.npmjs.org_caseless___caseless_0.12.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_caseless___caseless_0.12.0.tgz";
        url  = "https://registry.npmjs.org/caseless/-/caseless-0.12.0.tgz";
        sha1 = "1b681c21ff84033c826543090689420d187151dc";
      };
    }
    {
      name = "https___registry.npmjs.org_chalk___chalk_1.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_chalk___chalk_1.1.3.tgz";
        url  = "https://registry.npmjs.org/chalk/-/chalk-1.1.3.tgz";
        sha1 = "a8115c55e4a702fe4d150abd3872822a7e09fc98";
      };
    }
    {
      name = "https___registry.npmjs.org_chalk___chalk_2.4.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_chalk___chalk_2.4.2.tgz";
        url  = "https://registry.npmjs.org/chalk/-/chalk-2.4.2.tgz";
        sha1 = "cd42541677a54333cf541a49108c1432b44c9424";
      };
    }
    {
      name = "https___registry.npmjs.org_chalk___chalk_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_chalk___chalk_3.0.0.tgz";
        url  = "https://registry.npmjs.org/chalk/-/chalk-3.0.0.tgz";
        sha1 = "3f73c2bf526591f574cc492c51e2456349f844e4";
      };
    }
    {
      name = "https___registry.npmjs.org_chokidar___chokidar_2.1.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_chokidar___chokidar_2.1.8.tgz";
        url  = "https://registry.npmjs.org/chokidar/-/chokidar-2.1.8.tgz";
        sha1 = "804b3a7b6a99358c3c5c61e71d8728f041cff917";
      };
    }
    {
      name = "https___registry.npmjs.org_ci_info___ci_info_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ci_info___ci_info_2.0.0.tgz";
        url  = "https://registry.npmjs.org/ci-info/-/ci-info-2.0.0.tgz";
        sha1 = "67a9e964be31a51e15e5010d58e6f12834002f46";
      };
    }
    {
      name = "https___registry.npmjs.org_cipher_base___cipher_base_1.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cipher_base___cipher_base_1.0.4.tgz";
        url  = "https://registry.npmjs.org/cipher-base/-/cipher-base-1.0.4.tgz";
        sha1 = "8760e4ecc272f4c363532f926d874aae2c1397de";
      };
    }
    {
      name = "https___registry.npmjs.org_class_utils___class_utils_0.3.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_class_utils___class_utils_0.3.6.tgz";
        url  = "https://registry.npmjs.org/class-utils/-/class-utils-0.3.6.tgz";
        sha1 = "f93369ae8b9a7ce02fd41faad0ca83033190c463";
      };
    }
    {
      name = "https___registry.npmjs.org_cli_cursor___cli_cursor_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cli_cursor___cli_cursor_2.1.0.tgz";
        url  = "https://registry.npmjs.org/cli-cursor/-/cli-cursor-2.1.0.tgz";
        sha1 = "b35dac376479facc3e94747d41d0d0f5238ffcb5";
      };
    }
    {
      name = "https___registry.npmjs.org_cli_spinners___cli_spinners_1.3.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cli_spinners___cli_spinners_1.3.1.tgz";
        url  = "https://registry.npmjs.org/cli-spinners/-/cli-spinners-1.3.1.tgz";
        sha1 = "002c1990912d0d59580c93bd36c056de99e4259a";
      };
    }
    {
      name = "https___registry.npmjs.org_cliui___cliui_6.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cliui___cliui_6.0.0.tgz";
        url  = "https://registry.npmjs.org/cliui/-/cliui-6.0.0.tgz";
        sha1 = "511d702c0c4e41ca156d7d0e96021f23e13225b1";
      };
    }
    {
      name = "https___registry.npmjs.org_clone___clone_1.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_clone___clone_1.0.4.tgz";
        url  = "https://registry.npmjs.org/clone/-/clone-1.0.4.tgz";
        sha1 = "da309cc263df15994c688ca902179ca3c7cd7c7e";
      };
    }
    {
      name = "https___registry.npmjs.org_clone___clone_2.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_clone___clone_2.1.2.tgz";
        url  = "https://registry.npmjs.org/clone/-/clone-2.1.2.tgz";
        sha1 = "1b7f4b9f591f1e8f83670401600345a02887435f";
      };
    }
    {
      name = "https___registry.npmjs.org_co___co_4.6.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_co___co_4.6.0.tgz";
        url  = "https://registry.npmjs.org/co/-/co-4.6.0.tgz";
        sha1 = "6ea6bdf3d853ae54ccb8e47bfa0bf3f9031fb184";
      };
    }
    {
      name = "https___registry.npmjs.org_coa___coa_2.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_coa___coa_2.0.2.tgz";
        url  = "https://registry.npmjs.org/coa/-/coa-2.0.2.tgz";
        sha1 = "43f6c21151b4ef2bf57187db0d73de229e3e7ec3";
      };
    }
    {
      name = "https___registry.npmjs.org_collect_v8_coverage___collect_v8_coverage_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_collect_v8_coverage___collect_v8_coverage_1.0.1.tgz";
        url  = "https://registry.npmjs.org/collect-v8-coverage/-/collect-v8-coverage-1.0.1.tgz";
        sha1 = "cc2c8e94fc18bbdffe64d6534570c8a673b27f59";
      };
    }
    {
      name = "https___registry.npmjs.org_collection_visit___collection_visit_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_collection_visit___collection_visit_1.0.0.tgz";
        url  = "https://registry.npmjs.org/collection-visit/-/collection-visit-1.0.0.tgz";
        sha1 = "4bc0373c164bc3291b4d368c829cf1a80a59dca0";
      };
    }
    {
      name = "https___registry.npmjs.org_color_convert___color_convert_1.9.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_color_convert___color_convert_1.9.3.tgz";
        url  = "https://registry.npmjs.org/color-convert/-/color-convert-1.9.3.tgz";
        sha1 = "bb71850690e1f136567de629d2d5471deda4c1e8";
      };
    }
    {
      name = "https___registry.npmjs.org_color_convert___color_convert_2.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_color_convert___color_convert_2.0.1.tgz";
        url  = "https://registry.npmjs.org/color-convert/-/color-convert-2.0.1.tgz";
        sha1 = "72d3a68d598c9bdb3af2ad1e84f21d896abd4de3";
      };
    }
    {
      name = "https___registry.npmjs.org_color_name___color_name_1.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_color_name___color_name_1.1.3.tgz";
        url  = "https://registry.npmjs.org/color-name/-/color-name-1.1.3.tgz";
        sha1 = "a7d0558bd89c42f795dd42328f740831ca53bc25";
      };
    }
    {
      name = "https___registry.npmjs.org_color_name___color_name_1.1.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_color_name___color_name_1.1.4.tgz";
        url  = "https://registry.npmjs.org/color-name/-/color-name-1.1.4.tgz";
        sha1 = "c2a09a87acbde69543de6f63fa3995c826c536a2";
      };
    }
    {
      name = "https___registry.npmjs.org_color_string___color_string_1.5.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_color_string___color_string_1.5.4.tgz";
        url  = "https://registry.npmjs.org/color-string/-/color-string-1.5.4.tgz";
        sha1 = "dd51cd25cfee953d138fe4002372cc3d0e504cb6";
      };
    }
    {
      name = "https___registry.npmjs.org_color___color_3.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_color___color_3.1.3.tgz";
        url  = "https://registry.npmjs.org/color/-/color-3.1.3.tgz";
        sha1 = "ca67fb4e7b97d611dcde39eceed422067d91596e";
      };
    }
    {
      name = "https___registry.npmjs.org_colorette___colorette_1.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_colorette___colorette_1.2.1.tgz";
        url  = "https://registry.npmjs.org/colorette/-/colorette-1.2.1.tgz";
        sha1 = "4d0b921325c14faf92633086a536db6e89564b1b";
      };
    }
    {
      name = "https___registry.npmjs.org_combined_stream___combined_stream_1.0.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_combined_stream___combined_stream_1.0.8.tgz";
        url  = "https://registry.npmjs.org/combined-stream/-/combined-stream-1.0.8.tgz";
        sha1 = "c3d45a8b34fd730631a110a8a2520682b31d5a7f";
      };
    }
    {
      name = "https___registry.npmjs.org_command_exists___command_exists_1.2.9.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_command_exists___command_exists_1.2.9.tgz";
        url  = "https://registry.npmjs.org/command-exists/-/command-exists-1.2.9.tgz";
        sha1 = "c50725af3808c8ab0260fd60b01fbfa25b954f69";
      };
    }
    {
      name = "https___registry.npmjs.org_commander___commander_2.20.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_commander___commander_2.20.3.tgz";
        url  = "https://registry.npmjs.org/commander/-/commander-2.20.3.tgz";
        sha1 = "fd485e84c03eb4881c20722ba48035e8531aeb33";
      };
    }
    {
      name = "https___registry.npmjs.org_commander___commander_5.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_commander___commander_5.1.0.tgz";
        url  = "https://registry.npmjs.org/commander/-/commander-5.1.0.tgz";
        sha1 = "46abbd1652f8e059bddaef99bbdcb2ad9cf179ae";
      };
    }
    {
      name = "https___registry.npmjs.org_component_emitter___component_emitter_1.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_component_emitter___component_emitter_1.3.0.tgz";
        url  = "https://registry.npmjs.org/component-emitter/-/component-emitter-1.3.0.tgz";
        sha1 = "16e4070fba8ae29b679f2215853ee181ab2eabc0";
      };
    }
    {
      name = "https___registry.npmjs.org_concat_map___concat_map_0.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_concat_map___concat_map_0.0.1.tgz";
        url  = "https://registry.npmjs.org/concat-map/-/concat-map-0.0.1.tgz";
        sha1 = "d8a96bd77fd68df7793a73036a3ba0d5405d477b";
      };
    }
    {
      name = "https___registry.npmjs.org_concat_stream___concat_stream_1.6.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_concat_stream___concat_stream_1.6.2.tgz";
        url  = "https://registry.npmjs.org/concat-stream/-/concat-stream-1.6.2.tgz";
        sha1 = "904bdf194cd3122fc675c77fc4ac3d4ff0fd1a34";
      };
    }
    {
      name = "https___registry.npmjs.org_console_browserify___console_browserify_1.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_console_browserify___console_browserify_1.2.0.tgz";
        url  = "https://registry.npmjs.org/console-browserify/-/console-browserify-1.2.0.tgz";
        sha1 = "67063cef57ceb6cf4993a2ab3a55840ae8c49336";
      };
    }
    {
      name = "https___registry.npmjs.org_constants_browserify___constants_browserify_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_constants_browserify___constants_browserify_1.0.0.tgz";
        url  = "https://registry.npmjs.org/constants-browserify/-/constants-browserify-1.0.0.tgz";
        sha1 = "c20b96d8c617748aaf1c16021760cd27fcb8cb75";
      };
    }
    {
      name = "https___registry.npmjs.org_convert_source_map___convert_source_map_1.7.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_convert_source_map___convert_source_map_1.7.0.tgz";
        url  = "https://registry.npmjs.org/convert-source-map/-/convert-source-map-1.7.0.tgz";
        sha1 = "17a2cb882d7f77d3490585e2ce6c524424a3a442";
      };
    }
    {
      name = "https___registry.npmjs.org_copy_descriptor___copy_descriptor_0.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_copy_descriptor___copy_descriptor_0.1.1.tgz";
        url  = "https://registry.npmjs.org/copy-descriptor/-/copy-descriptor-0.1.1.tgz";
        sha1 = "676f6eb3c39997c2ee1ac3a924fd6124748f578d";
      };
    }
    {
      name = "https___registry.npmjs.org_core_js_compat___core_js_compat_3.8.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_core_js_compat___core_js_compat_3.8.1.tgz";
        url  = "https://registry.npmjs.org/core-js-compat/-/core-js-compat-3.8.1.tgz";
        sha1 = "8d1ddd341d660ba6194cbe0ce60f4c794c87a36e";
      };
    }
    {
      name = "https___registry.npmjs.org_core_js___core_js_2.6.12.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_core_js___core_js_2.6.12.tgz";
        url  = "https://registry.npmjs.org/core-js/-/core-js-2.6.12.tgz";
        sha1 = "d9333dfa7b065e347cc5682219d6f690859cc2ec";
      };
    }
    {
      name = "https___registry.npmjs.org_core_util_is___core_util_is_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_core_util_is___core_util_is_1.0.2.tgz";
        url  = "https://registry.npmjs.org/core-util-is/-/core-util-is-1.0.2.tgz";
        sha1 = "b5fd54220aa2bc5ab57aab7140c940754503c1a7";
      };
    }
    {
      name = "https___registry.npmjs.org_cosmiconfig___cosmiconfig_5.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cosmiconfig___cosmiconfig_5.2.1.tgz";
        url  = "https://registry.npmjs.org/cosmiconfig/-/cosmiconfig-5.2.1.tgz";
        sha1 = "040f726809c591e77a17c0a3626ca45b4f168b1a";
      };
    }
    {
      name = "https___registry.npmjs.org_create_ecdh___create_ecdh_4.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_create_ecdh___create_ecdh_4.0.4.tgz";
        url  = "https://registry.npmjs.org/create-ecdh/-/create-ecdh-4.0.4.tgz";
        sha1 = "d6e7f4bffa66736085a0762fd3a632684dabcc4e";
      };
    }
    {
      name = "https___registry.npmjs.org_create_hash___create_hash_1.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_create_hash___create_hash_1.2.0.tgz";
        url  = "https://registry.npmjs.org/create-hash/-/create-hash-1.2.0.tgz";
        sha1 = "889078af11a63756bcfb59bd221996be3a9ef196";
      };
    }
    {
      name = "https___registry.npmjs.org_create_hmac___create_hmac_1.1.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_create_hmac___create_hmac_1.1.7.tgz";
        url  = "https://registry.npmjs.org/create-hmac/-/create-hmac-1.1.7.tgz";
        sha1 = "69170c78b3ab957147b2b8b04572e47ead2243ff";
      };
    }
    {
      name = "https___registry.npmjs.org_cross_spawn___cross_spawn_6.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cross_spawn___cross_spawn_6.0.5.tgz";
        url  = "https://registry.npmjs.org/cross-spawn/-/cross-spawn-6.0.5.tgz";
        sha1 = "4a5ec7c64dfae22c3a14124dbacdee846d80cbc4";
      };
    }
    {
      name = "https___registry.npmjs.org_cross_spawn___cross_spawn_7.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cross_spawn___cross_spawn_7.0.3.tgz";
        url  = "https://registry.npmjs.org/cross-spawn/-/cross-spawn-7.0.3.tgz";
        sha1 = "f73a85b9d5d41d045551c177e2882d4ac85728a6";
      };
    }
    {
      name = "https___registry.npmjs.org_crypto_browserify___crypto_browserify_3.12.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_crypto_browserify___crypto_browserify_3.12.0.tgz";
        url  = "https://registry.npmjs.org/crypto-browserify/-/crypto-browserify-3.12.0.tgz";
        sha1 = "396cf9f3137f03e4b8e532c58f698254e00f80ec";
      };
    }
    {
      name = "https___registry.npmjs.org_css_color_names___css_color_names_0.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_css_color_names___css_color_names_0.0.4.tgz";
        url  = "https://registry.npmjs.org/css-color-names/-/css-color-names-0.0.4.tgz";
        sha1 = "808adc2e79cf84738069b646cb20ec27beb629e0";
      };
    }
    {
      name = "https___registry.npmjs.org_css_declaration_sorter___css_declaration_sorter_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_css_declaration_sorter___css_declaration_sorter_4.0.1.tgz";
        url  = "https://registry.npmjs.org/css-declaration-sorter/-/css-declaration-sorter-4.0.1.tgz";
        sha1 = "c198940f63a76d7e36c1e71018b001721054cb22";
      };
    }
    {
      name = "https___registry.npmjs.org_css_modules_loader_core___css_modules_loader_core_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_css_modules_loader_core___css_modules_loader_core_1.1.0.tgz";
        url  = "https://registry.npmjs.org/css-modules-loader-core/-/css-modules-loader-core-1.1.0.tgz";
        sha1 = "5908668294a1becd261ae0a4ce21b0b551f21d16";
      };
    }
    {
      name = "https___registry.npmjs.org_css_select_base_adapter___css_select_base_adapter_0.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_css_select_base_adapter___css_select_base_adapter_0.1.1.tgz";
        url  = "https://registry.npmjs.org/css-select-base-adapter/-/css-select-base-adapter-0.1.1.tgz";
        sha1 = "3b2ff4972cc362ab88561507a95408a1432135d7";
      };
    }
    {
      name = "https___registry.npmjs.org_css_select___css_select_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_css_select___css_select_2.1.0.tgz";
        url  = "https://registry.npmjs.org/css-select/-/css-select-2.1.0.tgz";
        sha1 = "6a34653356635934a81baca68d0255432105dbef";
      };
    }
    {
      name = "https___registry.npmjs.org_css_selector_tokenizer___css_selector_tokenizer_0.7.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_css_selector_tokenizer___css_selector_tokenizer_0.7.3.tgz";
        url  = "https://registry.npmjs.org/css-selector-tokenizer/-/css-selector-tokenizer-0.7.3.tgz";
        sha1 = "735f26186e67c749aaf275783405cf0661fae8f1";
      };
    }
    {
      name = "https___registry.npmjs.org_css_tree___css_tree_1.0.0_alpha.37.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_css_tree___css_tree_1.0.0_alpha.37.tgz";
        url  = "https://registry.npmjs.org/css-tree/-/css-tree-1.0.0-alpha.37.tgz";
        sha1 = "98bebd62c4c1d9f960ec340cf9f7522e30709a22";
      };
    }
    {
      name = "https___registry.npmjs.org_css_tree___css_tree_1.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_css_tree___css_tree_1.1.2.tgz";
        url  = "https://registry.npmjs.org/css-tree/-/css-tree-1.1.2.tgz";
        sha1 = "9ae393b5dafd7dae8a622475caec78d3d8fbd7b5";
      };
    }
    {
      name = "https___registry.npmjs.org_css_what___css_what_3.4.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_css_what___css_what_3.4.2.tgz";
        url  = "https://registry.npmjs.org/css-what/-/css-what-3.4.2.tgz";
        sha1 = "ea7026fcb01777edbde52124e21f327e7ae950e4";
      };
    }
    {
      name = "https___registry.npmjs.org_cssesc___cssesc_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cssesc___cssesc_3.0.0.tgz";
        url  = "https://registry.npmjs.org/cssesc/-/cssesc-3.0.0.tgz";
        sha1 = "37741919903b868565e1c09ea747445cd18983ee";
      };
    }
    {
      name = "https___registry.npmjs.org_cssnano_preset_default___cssnano_preset_default_4.0.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cssnano_preset_default___cssnano_preset_default_4.0.7.tgz";
        url  = "https://registry.npmjs.org/cssnano-preset-default/-/cssnano-preset-default-4.0.7.tgz";
        sha1 = "51ec662ccfca0f88b396dcd9679cdb931be17f76";
      };
    }
    {
      name = "https___registry.npmjs.org_cssnano_util_get_arguments___cssnano_util_get_arguments_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cssnano_util_get_arguments___cssnano_util_get_arguments_4.0.0.tgz";
        url  = "https://registry.npmjs.org/cssnano-util-get-arguments/-/cssnano-util-get-arguments-4.0.0.tgz";
        sha1 = "ed3a08299f21d75741b20f3b81f194ed49cc150f";
      };
    }
    {
      name = "https___registry.npmjs.org_cssnano_util_get_match___cssnano_util_get_match_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cssnano_util_get_match___cssnano_util_get_match_4.0.0.tgz";
        url  = "https://registry.npmjs.org/cssnano-util-get-match/-/cssnano-util-get-match-4.0.0.tgz";
        sha1 = "c0e4ca07f5386bb17ec5e52250b4f5961365156d";
      };
    }
    {
      name = "https___registry.npmjs.org_cssnano_util_raw_cache___cssnano_util_raw_cache_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cssnano_util_raw_cache___cssnano_util_raw_cache_4.0.1.tgz";
        url  = "https://registry.npmjs.org/cssnano-util-raw-cache/-/cssnano-util-raw-cache-4.0.1.tgz";
        sha1 = "b26d5fd5f72a11dfe7a7846fb4c67260f96bf282";
      };
    }
    {
      name = "https___registry.npmjs.org_cssnano_util_same_parent___cssnano_util_same_parent_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cssnano_util_same_parent___cssnano_util_same_parent_4.0.1.tgz";
        url  = "https://registry.npmjs.org/cssnano-util-same-parent/-/cssnano-util-same-parent-4.0.1.tgz";
        sha1 = "574082fb2859d2db433855835d9a8456ea18bbf3";
      };
    }
    {
      name = "https___registry.npmjs.org_cssnano___cssnano_4.1.10.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cssnano___cssnano_4.1.10.tgz";
        url  = "https://registry.npmjs.org/cssnano/-/cssnano-4.1.10.tgz";
        sha1 = "0ac41f0b13d13d465487e111b778d42da631b8b2";
      };
    }
    {
      name = "https___registry.npmjs.org_csso___csso_4.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_csso___csso_4.2.0.tgz";
        url  = "https://registry.npmjs.org/csso/-/csso-4.2.0.tgz";
        sha1 = "ea3a561346e8dc9f546d6febedd50187cf389529";
      };
    }
    {
      name = "https___registry.npmjs.org_cssom___cssom_0.3.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cssom___cssom_0.3.8.tgz";
        url  = "https://registry.npmjs.org/cssom/-/cssom-0.3.8.tgz";
        sha1 = "9f1276f5b2b463f2114d3f2c75250af8c1a36f4a";
      };
    }
    {
      name = "https___registry.npmjs.org_cssom___cssom_0.4.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cssom___cssom_0.4.4.tgz";
        url  = "https://registry.npmjs.org/cssom/-/cssom-0.4.4.tgz";
        sha1 = "5a66cf93d2d0b661d80bf6a44fb65f5c2e4e0a10";
      };
    }
    {
      name = "https___registry.npmjs.org_cssstyle___cssstyle_1.4.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cssstyle___cssstyle_1.4.0.tgz";
        url  = "https://registry.npmjs.org/cssstyle/-/cssstyle-1.4.0.tgz";
        sha1 = "9d31328229d3c565c61e586b02041a28fccdccf1";
      };
    }
    {
      name = "https___registry.npmjs.org_cssstyle___cssstyle_2.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cssstyle___cssstyle_2.3.0.tgz";
        url  = "https://registry.npmjs.org/cssstyle/-/cssstyle-2.3.0.tgz";
        sha1 = "ff665a0ddbdc31864b09647f34163443d90b0852";
      };
    }
    {
      name = "https___registry.npmjs.org_dashdash___dashdash_1.14.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_dashdash___dashdash_1.14.1.tgz";
        url  = "https://registry.npmjs.org/dashdash/-/dashdash-1.14.1.tgz";
        sha1 = "853cfa0f7cbe2fed5de20326b8dd581035f6e2f0";
      };
    }
    {
      name = "https___registry.npmjs.org_data_urls___data_urls_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_data_urls___data_urls_1.1.0.tgz";
        url  = "https://registry.npmjs.org/data-urls/-/data-urls-1.1.0.tgz";
        sha1 = "15ee0582baa5e22bb59c77140da8f9c76963bbfe";
      };
    }
    {
      name = "https___registry.npmjs.org_deasync___deasync_0.1.21.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_deasync___deasync_0.1.21.tgz";
        url  = "https://registry.npmjs.org/deasync/-/deasync-0.1.21.tgz";
        sha1 = "bb11eabd4466c0d8776f0d82deb8a6126460d30f";
      };
    }
    {
      name = "https___registry.npmjs.org_debug___debug_2.6.9.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_debug___debug_2.6.9.tgz";
        url  = "https://registry.npmjs.org/debug/-/debug-2.6.9.tgz";
        sha1 = "5d128515df134ff327e90a4c93f4e077a536341f";
      };
    }
    {
      name = "https___registry.npmjs.org_debug___debug_4.3.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_debug___debug_4.3.1.tgz";
        url  = "https://registry.npmjs.org/debug/-/debug-4.3.1.tgz";
        sha1 = "f0d229c505e0c6d8c49ac553d1b13dc183f6b2ee";
      };
    }
    {
      name = "https___registry.npmjs.org_decamelize___decamelize_1.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_decamelize___decamelize_1.2.0.tgz";
        url  = "https://registry.npmjs.org/decamelize/-/decamelize-1.2.0.tgz";
        sha1 = "f6534d15148269b20352e7bee26f501f9a191290";
      };
    }
    {
      name = "https___registry.npmjs.org_decode_uri_component___decode_uri_component_0.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_decode_uri_component___decode_uri_component_0.2.0.tgz";
        url  = "https://registry.npmjs.org/decode-uri-component/-/decode-uri-component-0.2.0.tgz";
        sha1 = "eb3913333458775cb84cd1a1fae062106bb87545";
      };
    }
    {
      name = "https___registry.npmjs.org_deep_is___deep_is_0.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_deep_is___deep_is_0.1.3.tgz";
        url  = "https://registry.npmjs.org/deep-is/-/deep-is-0.1.3.tgz";
        sha1 = "b369d6fb5dbc13eecf524f91b070feedc357cf34";
      };
    }
    {
      name = "https___registry.npmjs.org_deepmerge___deepmerge_4.2.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_deepmerge___deepmerge_4.2.2.tgz";
        url  = "https://registry.npmjs.org/deepmerge/-/deepmerge-4.2.2.tgz";
        sha1 = "44d2ea3679b8f4d4ffba33f03d865fc1e7bf4955";
      };
    }
    {
      name = "https___registry.npmjs.org_defaults___defaults_1.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_defaults___defaults_1.0.3.tgz";
        url  = "https://registry.npmjs.org/defaults/-/defaults-1.0.3.tgz";
        sha1 = "c656051e9817d9ff08ed881477f3fe4019f3ef7d";
      };
    }
    {
      name = "https___registry.npmjs.org_define_properties___define_properties_1.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_define_properties___define_properties_1.1.3.tgz";
        url  = "https://registry.npmjs.org/define-properties/-/define-properties-1.1.3.tgz";
        sha1 = "cf88da6cbee26fe6db7094f61d870cbd84cee9f1";
      };
    }
    {
      name = "https___registry.npmjs.org_define_property___define_property_0.2.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_define_property___define_property_0.2.5.tgz";
        url  = "https://registry.npmjs.org/define-property/-/define-property-0.2.5.tgz";
        sha1 = "c35b1ef918ec3c990f9a5bc57be04aacec5c8116";
      };
    }
    {
      name = "https___registry.npmjs.org_define_property___define_property_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_define_property___define_property_1.0.0.tgz";
        url  = "https://registry.npmjs.org/define-property/-/define-property-1.0.0.tgz";
        sha1 = "769ebaaf3f4a63aad3af9e8d304c9bbe79bfb0e6";
      };
    }
    {
      name = "https___registry.npmjs.org_define_property___define_property_2.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_define_property___define_property_2.0.2.tgz";
        url  = "https://registry.npmjs.org/define-property/-/define-property-2.0.2.tgz";
        sha1 = "d459689e8d654ba77e02a817f8710d702cb16e9d";
      };
    }
    {
      name = "https___registry.npmjs.org_delayed_stream___delayed_stream_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_delayed_stream___delayed_stream_1.0.0.tgz";
        url  = "https://registry.npmjs.org/delayed-stream/-/delayed-stream-1.0.0.tgz";
        sha1 = "df3ae199acadfb7d440aaae0b29e2272b24ec619";
      };
    }
    {
      name = "https___registry.npmjs.org_depd___depd_1.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_depd___depd_1.1.2.tgz";
        url  = "https://registry.npmjs.org/depd/-/depd-1.1.2.tgz";
        sha1 = "9bcd52e14c097763e749b274c4346ed2e560b5a9";
      };
    }
    {
      name = "https___registry.npmjs.org_des.js___des.js_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_des.js___des.js_1.0.1.tgz";
        url  = "https://registry.npmjs.org/des.js/-/des.js-1.0.1.tgz";
        sha1 = "5382142e1bdc53f85d86d53e5f4aa7deb91e0843";
      };
    }
    {
      name = "https___registry.npmjs.org_destroy___destroy_1.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_destroy___destroy_1.0.4.tgz";
        url  = "https://registry.npmjs.org/destroy/-/destroy-1.0.4.tgz";
        sha1 = "978857442c44749e4206613e37946205826abd80";
      };
    }
    {
      name = "https___registry.npmjs.org_detect_newline___detect_newline_3.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_detect_newline___detect_newline_3.1.0.tgz";
        url  = "https://registry.npmjs.org/detect-newline/-/detect-newline-3.1.0.tgz";
        sha1 = "576f5dfc63ae1a192ff192d8ad3af6308991b651";
      };
    }
    {
      name = "https___registry.npmjs.org_diff_sequences___diff_sequences_25.2.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_diff_sequences___diff_sequences_25.2.6.tgz";
        url  = "https://registry.npmjs.org/diff-sequences/-/diff-sequences-25.2.6.tgz";
        sha1 = "5f467c00edd35352b7bca46d7927d60e687a76dd";
      };
    }
    {
      name = "https___registry.npmjs.org_diffie_hellman___diffie_hellman_5.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_diffie_hellman___diffie_hellman_5.0.3.tgz";
        url  = "https://registry.npmjs.org/diffie-hellman/-/diffie-hellman-5.0.3.tgz";
        sha1 = "40e8ee98f55a2149607146921c63e1ae5f3d2875";
      };
    }
    {
      name = "https___registry.npmjs.org_dom_serializer___dom_serializer_0.2.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_dom_serializer___dom_serializer_0.2.2.tgz";
        url  = "https://registry.npmjs.org/dom-serializer/-/dom-serializer-0.2.2.tgz";
        sha1 = "1afb81f533717175d478655debc5e332d9f9bb51";
      };
    }
    {
      name = "https___registry.npmjs.org_domain_browser___domain_browser_1.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_domain_browser___domain_browser_1.2.0.tgz";
        url  = "https://registry.npmjs.org/domain-browser/-/domain-browser-1.2.0.tgz";
        sha1 = "3d31f50191a6749dd1375a7f522e823d42e54eda";
      };
    }
    {
      name = "https___registry.npmjs.org_domelementtype___domelementtype_1.3.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_domelementtype___domelementtype_1.3.1.tgz";
        url  = "https://registry.npmjs.org/domelementtype/-/domelementtype-1.3.1.tgz";
        sha1 = "d048c44b37b0d10a7f2a3d5fee3f4333d790481f";
      };
    }
    {
      name = "https___registry.npmjs.org_domelementtype___domelementtype_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_domelementtype___domelementtype_2.1.0.tgz";
        url  = "https://registry.npmjs.org/domelementtype/-/domelementtype-2.1.0.tgz";
        sha1 = "a851c080a6d1c3d94344aed151d99f669edf585e";
      };
    }
    {
      name = "https___registry.npmjs.org_domexception___domexception_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_domexception___domexception_1.0.1.tgz";
        url  = "https://registry.npmjs.org/domexception/-/domexception-1.0.1.tgz";
        sha1 = "937442644ca6a31261ef36e3ec677fe805582c90";
      };
    }
    {
      name = "https___registry.npmjs.org_domhandler___domhandler_2.4.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_domhandler___domhandler_2.4.2.tgz";
        url  = "https://registry.npmjs.org/domhandler/-/domhandler-2.4.2.tgz";
        sha1 = "8805097e933d65e85546f726d60f5eb88b44f803";
      };
    }
    {
      name = "https___registry.npmjs.org_domutils___domutils_1.7.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_domutils___domutils_1.7.0.tgz";
        url  = "https://registry.npmjs.org/domutils/-/domutils-1.7.0.tgz";
        sha1 = "56ea341e834e06e6748af7a1cb25da67ea9f8c2a";
      };
    }
    {
      name = "https___registry.npmjs.org_dot_prop___dot_prop_5.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_dot_prop___dot_prop_5.3.0.tgz";
        url  = "https://registry.npmjs.org/dot-prop/-/dot-prop-5.3.0.tgz";
        sha1 = "90ccce708cd9cd82cc4dc8c3ddd9abdd55b20e88";
      };
    }
    {
      name = "https___registry.npmjs.org_dotenv_expand___dotenv_expand_5.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_dotenv_expand___dotenv_expand_5.1.0.tgz";
        url  = "https://registry.npmjs.org/dotenv-expand/-/dotenv-expand-5.1.0.tgz";
        sha1 = "3fbaf020bfd794884072ea26b1e9791d45a629f0";
      };
    }
    {
      name = "https___registry.npmjs.org_dotenv___dotenv_5.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_dotenv___dotenv_5.0.1.tgz";
        url  = "https://registry.npmjs.org/dotenv/-/dotenv-5.0.1.tgz";
        sha1 = "a5317459bd3d79ab88cff6e44057a6a3fbb1fcef";
      };
    }
    {
      name = "https___registry.npmjs.org_duplexer2___duplexer2_0.1.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_duplexer2___duplexer2_0.1.4.tgz";
        url  = "https://registry.npmjs.org/duplexer2/-/duplexer2-0.1.4.tgz";
        sha1 = "8b12dab878c0d69e3e7891051662a32fc6bddcc1";
      };
    }
    {
      name = "https___registry.npmjs.org_ecc_jsbn___ecc_jsbn_0.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ecc_jsbn___ecc_jsbn_0.1.2.tgz";
        url  = "https://registry.npmjs.org/ecc-jsbn/-/ecc-jsbn-0.1.2.tgz";
        sha1 = "3a83a904e54353287874c564b7549386849a98c9";
      };
    }
    {
      name = "https___registry.npmjs.org_ee_first___ee_first_1.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ee_first___ee_first_1.1.1.tgz";
        url  = "https://registry.npmjs.org/ee-first/-/ee-first-1.1.1.tgz";
        sha1 = "590c61156b0ae2f4f0255732a158b266bc56b21d";
      };
    }
    {
      name = "https___registry.npmjs.org_electron_to_chromium___electron_to_chromium_1.3.620.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_electron_to_chromium___electron_to_chromium_1.3.620.tgz";
        url  = "https://registry.npmjs.org/electron-to-chromium/-/electron-to-chromium-1.3.620.tgz";
        sha1 = "c6f36a7e398acc9d7d12743a6f58d536fbc58700";
      };
    }
    {
      name = "https___registry.npmjs.org_elliptic___elliptic_6.5.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_elliptic___elliptic_6.5.3.tgz";
        url  = "https://registry.npmjs.org/elliptic/-/elliptic-6.5.3.tgz";
        sha1 = "cb59eb2efdaf73a0bd78ccd7015a62ad6e0f93d6";
      };
    }
    {
      name = "https___registry.npmjs.org_emoji_regex___emoji_regex_8.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_emoji_regex___emoji_regex_8.0.0.tgz";
        url  = "https://registry.npmjs.org/emoji-regex/-/emoji-regex-8.0.0.tgz";
        sha1 = "e818fd69ce5ccfcb404594f842963bf53164cc37";
      };
    }
    {
      name = "https___registry.npmjs.org_encodeurl___encodeurl_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_encodeurl___encodeurl_1.0.2.tgz";
        url  = "https://registry.npmjs.org/encodeurl/-/encodeurl-1.0.2.tgz";
        sha1 = "ad3ff4c86ec2d029322f5a02c3a9a606c95b3f59";
      };
    }
    {
      name = "https___registry.npmjs.org_end_of_stream___end_of_stream_1.4.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_end_of_stream___end_of_stream_1.4.4.tgz";
        url  = "https://registry.npmjs.org/end-of-stream/-/end-of-stream-1.4.4.tgz";
        sha1 = "5ae64a5f45057baf3626ec14da0ca5e4b2431eb0";
      };
    }
    {
      name = "https___registry.npmjs.org_entities___entities_1.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_entities___entities_1.1.2.tgz";
        url  = "https://registry.npmjs.org/entities/-/entities-1.1.2.tgz";
        sha1 = "bdfa735299664dfafd34529ed4f8522a275fea56";
      };
    }
    {
      name = "https___registry.npmjs.org_entities___entities_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_entities___entities_2.1.0.tgz";
        url  = "https://registry.npmjs.org/entities/-/entities-2.1.0.tgz";
        sha1 = "992d3129cf7df6870b96c57858c249a120f8b8b5";
      };
    }
    {
      name = "https___registry.npmjs.org_envinfo___envinfo_7.7.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_envinfo___envinfo_7.7.3.tgz";
        url  = "https://registry.npmjs.org/envinfo/-/envinfo-7.7.3.tgz";
        sha1 = "4b2d8622e3e7366afb8091b23ed95569ea0208cc";
      };
    }
    {
      name = "https___registry.npmjs.org_error_ex___error_ex_1.3.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_error_ex___error_ex_1.3.2.tgz";
        url  = "https://registry.npmjs.org/error-ex/-/error-ex-1.3.2.tgz";
        sha1 = "b4ac40648107fdcdcfae242f428bea8a14d4f1bf";
      };
    }
    {
      name = "https___registry.npmjs.org_es_abstract___es_abstract_1.17.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_es_abstract___es_abstract_1.17.7.tgz";
        url  = "https://registry.npmjs.org/es-abstract/-/es-abstract-1.17.7.tgz";
        sha1 = "a4de61b2f66989fc7421676c1cb9787573ace54c";
      };
    }
    {
      name = "https___registry.npmjs.org_es_abstract___es_abstract_1.18.0_next.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_es_abstract___es_abstract_1.18.0_next.1.tgz";
        url  = "https://registry.npmjs.org/es-abstract/-/es-abstract-1.18.0-next.1.tgz";
        sha1 = "6e3a0a4bda717e5023ab3b8e90bec36108d22c68";
      };
    }
    {
      name = "https___registry.npmjs.org_es_to_primitive___es_to_primitive_1.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_es_to_primitive___es_to_primitive_1.2.1.tgz";
        url  = "https://registry.npmjs.org/es-to-primitive/-/es-to-primitive-1.2.1.tgz";
        sha1 = "e55cd4c9cdc188bcefb03b366c736323fc5c898a";
      };
    }
    {
      name = "https___registry.npmjs.org_escalade___escalade_3.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_escalade___escalade_3.1.1.tgz";
        url  = "https://registry.npmjs.org/escalade/-/escalade-3.1.1.tgz";
        sha1 = "d8cfdc7000965c5a0174b4a82eaa5c0552742e40";
      };
    }
    {
      name = "https___registry.npmjs.org_escape_html___escape_html_1.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_escape_html___escape_html_1.0.3.tgz";
        url  = "https://registry.npmjs.org/escape-html/-/escape-html-1.0.3.tgz";
        sha1 = "0258eae4d3d0c0974de1c169188ef0051d1d1988";
      };
    }
    {
      name = "https___registry.npmjs.org_escape_string_regexp___escape_string_regexp_1.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_escape_string_regexp___escape_string_regexp_1.0.5.tgz";
        url  = "https://registry.npmjs.org/escape-string-regexp/-/escape-string-regexp-1.0.5.tgz";
        sha1 = "1b61c0562190a8dff6ae3bb2cf0200ca130b86d4";
      };
    }
    {
      name = "https___registry.npmjs.org_escape_string_regexp___escape_string_regexp_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_escape_string_regexp___escape_string_regexp_2.0.0.tgz";
        url  = "https://registry.npmjs.org/escape-string-regexp/-/escape-string-regexp-2.0.0.tgz";
        sha1 = "a30304e99daa32e23b2fd20f51babd07cffca344";
      };
    }
    {
      name = "https___registry.npmjs.org_escodegen___escodegen_1.14.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_escodegen___escodegen_1.14.3.tgz";
        url  = "https://registry.npmjs.org/escodegen/-/escodegen-1.14.3.tgz";
        sha1 = "4e7b81fba61581dc97582ed78cab7f0e8d63f503";
      };
    }
    {
      name = "https___registry.npmjs.org_escodegen___escodegen_1.9.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_escodegen___escodegen_1.9.1.tgz";
        url  = "https://registry.npmjs.org/escodegen/-/escodegen-1.9.1.tgz";
        sha1 = "dbae17ef96c8e4bedb1356f4504fa4cc2f7cb7e2";
      };
    }
    {
      name = "https___registry.npmjs.org_esprima___esprima_3.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_esprima___esprima_3.1.3.tgz";
        url  = "https://registry.npmjs.org/esprima/-/esprima-3.1.3.tgz";
        sha1 = "fdca51cee6133895e3c88d535ce49dbff62a4633";
      };
    }
    {
      name = "https___registry.npmjs.org_esprima___esprima_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_esprima___esprima_4.0.1.tgz";
        url  = "https://registry.npmjs.org/esprima/-/esprima-4.0.1.tgz";
        sha1 = "13b04cdb3e6c5d19df91ab6987a8695619b0aa71";
      };
    }
    {
      name = "https___registry.npmjs.org_estraverse___estraverse_4.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_estraverse___estraverse_4.3.0.tgz";
        url  = "https://registry.npmjs.org/estraverse/-/estraverse-4.3.0.tgz";
        sha1 = "398ad3f3c5a24948be7725e83d11a7de28cdbd1d";
      };
    }
    {
      name = "https___registry.npmjs.org_esutils___esutils_2.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_esutils___esutils_2.0.3.tgz";
        url  = "https://registry.npmjs.org/esutils/-/esutils-2.0.3.tgz";
        sha1 = "74d2eb4de0b8da1293711910d50775b9b710ef64";
      };
    }
    {
      name = "https___registry.npmjs.org_etag___etag_1.8.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_etag___etag_1.8.1.tgz";
        url  = "https://registry.npmjs.org/etag/-/etag-1.8.1.tgz";
        sha1 = "41ae2eeb65efa62268aebfea83ac7d79299b0887";
      };
    }
    {
      name = "https___registry.npmjs.org_events___events_3.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_events___events_3.2.0.tgz";
        url  = "https://registry.npmjs.org/events/-/events-3.2.0.tgz";
        sha1 = "93b87c18f8efcd4202a461aec4dfc0556b639379";
      };
    }
    {
      name = "https___registry.npmjs.org_evp_bytestokey___evp_bytestokey_1.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_evp_bytestokey___evp_bytestokey_1.0.3.tgz";
        url  = "https://registry.npmjs.org/evp_bytestokey/-/evp_bytestokey-1.0.3.tgz";
        sha1 = "7fcbdb198dc71959432efe13842684e0525acb02";
      };
    }
    {
      name = "https___registry.npmjs.org_exec_sh___exec_sh_0.3.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_exec_sh___exec_sh_0.3.4.tgz";
        url  = "https://registry.npmjs.org/exec-sh/-/exec-sh-0.3.4.tgz";
        sha1 = "3a018ceb526cc6f6df2bb504b2bfe8e3a4934ec5";
      };
    }
    {
      name = "https___registry.npmjs.org_execa___execa_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_execa___execa_1.0.0.tgz";
        url  = "https://registry.npmjs.org/execa/-/execa-1.0.0.tgz";
        sha1 = "c6236a5bb4df6d6f15e88e7f017798216749ddd8";
      };
    }
    {
      name = "https___registry.npmjs.org_execa___execa_3.4.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_execa___execa_3.4.0.tgz";
        url  = "https://registry.npmjs.org/execa/-/execa-3.4.0.tgz";
        sha1 = "c08ed4550ef65d858fac269ffc8572446f37eb89";
      };
    }
    {
      name = "https___registry.npmjs.org_exit___exit_0.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_exit___exit_0.1.2.tgz";
        url  = "https://registry.npmjs.org/exit/-/exit-0.1.2.tgz";
        sha1 = "0632638f8d877cc82107d30a0fff1a17cba1cd0c";
      };
    }
    {
      name = "https___registry.npmjs.org_expand_brackets___expand_brackets_2.1.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_expand_brackets___expand_brackets_2.1.4.tgz";
        url  = "https://registry.npmjs.org/expand-brackets/-/expand-brackets-2.1.4.tgz";
        sha1 = "b77735e315ce30f6b6eff0f83b04151a22449622";
      };
    }
    {
      name = "https___registry.npmjs.org_expect___expect_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_expect___expect_25.5.0.tgz";
        url  = "https://registry.npmjs.org/expect/-/expect-25.5.0.tgz";
        sha1 = "f07f848712a2813bb59167da3fb828ca21f58bba";
      };
    }
    {
      name = "https___registry.npmjs.org_extend_shallow___extend_shallow_2.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_extend_shallow___extend_shallow_2.0.1.tgz";
        url  = "https://registry.npmjs.org/extend-shallow/-/extend-shallow-2.0.1.tgz";
        sha1 = "51af7d614ad9a9f610ea1bafbb989d6b1c56890f";
      };
    }
    {
      name = "https___registry.npmjs.org_extend_shallow___extend_shallow_3.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_extend_shallow___extend_shallow_3.0.2.tgz";
        url  = "https://registry.npmjs.org/extend-shallow/-/extend-shallow-3.0.2.tgz";
        sha1 = "26a71aaf073b39fb2127172746131c2704028db8";
      };
    }
    {
      name = "https___registry.npmjs.org_extend___extend_3.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_extend___extend_3.0.2.tgz";
        url  = "https://registry.npmjs.org/extend/-/extend-3.0.2.tgz";
        sha1 = "f8b1136b4071fbd8eb140aff858b1019ec2915fa";
      };
    }
    {
      name = "https___registry.npmjs.org_extglob___extglob_2.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_extglob___extglob_2.0.4.tgz";
        url  = "https://registry.npmjs.org/extglob/-/extglob-2.0.4.tgz";
        sha1 = "ad00fe4dc612a9232e8718711dc5cb5ab0285543";
      };
    }
    {
      name = "https___registry.npmjs.org_extsprintf___extsprintf_1.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_extsprintf___extsprintf_1.3.0.tgz";
        url  = "https://registry.npmjs.org/extsprintf/-/extsprintf-1.3.0.tgz";
        sha1 = "96918440e3041a7a414f8c52e3c574eb3c3e1e05";
      };
    }
    {
      name = "https___registry.npmjs.org_extsprintf___extsprintf_1.4.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_extsprintf___extsprintf_1.4.0.tgz";
        url  = "https://registry.npmjs.org/extsprintf/-/extsprintf-1.4.0.tgz";
        sha1 = "e2689f8f356fad62cca65a3a91c5df5f9551692f";
      };
    }
    {
      name = "https___registry.npmjs.org_falafel___falafel_2.2.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_falafel___falafel_2.2.4.tgz";
        url  = "https://registry.npmjs.org/falafel/-/falafel-2.2.4.tgz";
        sha1 = "b5d86c060c2412a43166243cb1bce44d1abd2819";
      };
    }
    {
      name = "https___registry.npmjs.org_fast_deep_equal___fast_deep_equal_3.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fast_deep_equal___fast_deep_equal_3.1.3.tgz";
        url  = "https://registry.npmjs.org/fast-deep-equal/-/fast-deep-equal-3.1.3.tgz";
        sha1 = "3a7d56b559d6cbc3eb512325244e619a65c6c525";
      };
    }
    {
      name = "https___registry.npmjs.org_fast_glob___fast_glob_2.2.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fast_glob___fast_glob_2.2.7.tgz";
        url  = "https://registry.npmjs.org/fast-glob/-/fast-glob-2.2.7.tgz";
        sha1 = "6953857c3afa475fff92ee6015d52da70a4cd39d";
      };
    }
    {
      name = "https___registry.npmjs.org_fast_json_stable_stringify___fast_json_stable_stringify_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fast_json_stable_stringify___fast_json_stable_stringify_2.1.0.tgz";
        url  = "https://registry.npmjs.org/fast-json-stable-stringify/-/fast-json-stable-stringify-2.1.0.tgz";
        sha1 = "874bf69c6f404c2b5d99c481341399fd55892633";
      };
    }
    {
      name = "https___registry.npmjs.org_fast_levenshtein___fast_levenshtein_2.0.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fast_levenshtein___fast_levenshtein_2.0.6.tgz";
        url  = "https://registry.npmjs.org/fast-levenshtein/-/fast-levenshtein-2.0.6.tgz";
        sha1 = "3d8a5c66883a16a30ca8643e851f19baa7797917";
      };
    }
    {
      name = "https___registry.npmjs.org_fastparse___fastparse_1.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fastparse___fastparse_1.1.2.tgz";
        url  = "https://registry.npmjs.org/fastparse/-/fastparse-1.1.2.tgz";
        sha1 = "91728c5a5942eced8531283c79441ee4122c35a9";
      };
    }
    {
      name = "https___registry.npmjs.org_fb_watchman___fb_watchman_2.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fb_watchman___fb_watchman_2.0.1.tgz";
        url  = "https://registry.npmjs.org/fb-watchman/-/fb-watchman-2.0.1.tgz";
        sha1 = "fc84fb39d2709cf3ff6d743706157bb5708a8a85";
      };
    }
    {
      name = "https___registry.npmjs.org_file_uri_to_path___file_uri_to_path_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_file_uri_to_path___file_uri_to_path_1.0.0.tgz";
        url  = "https://registry.npmjs.org/file-uri-to-path/-/file-uri-to-path-1.0.0.tgz";
        sha1 = "553a7b8446ff6f684359c445f1e37a05dacc33dd";
      };
    }
    {
      name = "https___registry.npmjs.org_filesize___filesize_3.6.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_filesize___filesize_3.6.1.tgz";
        url  = "https://registry.npmjs.org/filesize/-/filesize-3.6.1.tgz";
        sha1 = "090bb3ee01b6f801a8a8be99d31710b3422bb317";
      };
    }
    {
      name = "https___registry.npmjs.org_fill_range___fill_range_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fill_range___fill_range_4.0.0.tgz";
        url  = "https://registry.npmjs.org/fill-range/-/fill-range-4.0.0.tgz";
        sha1 = "d544811d428f98eb06a63dc402d2403c328c38f7";
      };
    }
    {
      name = "https___registry.npmjs.org_fill_range___fill_range_7.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fill_range___fill_range_7.0.1.tgz";
        url  = "https://registry.npmjs.org/fill-range/-/fill-range-7.0.1.tgz";
        sha1 = "1919a6a7c75fe38b2c7c77e5198535da9acdda40";
      };
    }
    {
      name = "https___registry.npmjs.org_find_up___find_up_4.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_find_up___find_up_4.1.0.tgz";
        url  = "https://registry.npmjs.org/find-up/-/find-up-4.1.0.tgz";
        sha1 = "97afe7d6cdc0bc5928584b7c8d7b16e8a9aa5d19";
      };
    }
    {
      name = "https___registry.npmjs.org_for_in___for_in_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_for_in___for_in_1.0.2.tgz";
        url  = "https://registry.npmjs.org/for-in/-/for-in-1.0.2.tgz";
        sha1 = "81068d295a8142ec0ac726c6e2200c30fb6d5e80";
      };
    }
    {
      name = "https___registry.npmjs.org_foreach___foreach_2.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_foreach___foreach_2.0.5.tgz";
        url  = "https://registry.npmjs.org/foreach/-/foreach-2.0.5.tgz";
        sha1 = "0bee005018aeb260d0a3af3ae658dd0136ec1b99";
      };
    }
    {
      name = "https___registry.npmjs.org_forever_agent___forever_agent_0.6.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_forever_agent___forever_agent_0.6.1.tgz";
        url  = "https://registry.npmjs.org/forever-agent/-/forever-agent-0.6.1.tgz";
        sha1 = "fbc71f0c41adeb37f96c577ad1ed42d8fdacca91";
      };
    }
    {
      name = "https___registry.npmjs.org_form_data___form_data_2.3.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_form_data___form_data_2.3.3.tgz";
        url  = "https://registry.npmjs.org/form-data/-/form-data-2.3.3.tgz";
        sha1 = "dcce52c05f644f298c6a7ab936bd724ceffbf3a6";
      };
    }
    {
      name = "https___registry.npmjs.org_fragment_cache___fragment_cache_0.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fragment_cache___fragment_cache_0.2.1.tgz";
        url  = "https://registry.npmjs.org/fragment-cache/-/fragment-cache-0.2.1.tgz";
        sha1 = "4290fad27f13e89be7f33799c6bc5a0abfff0d19";
      };
    }
    {
      name = "https___registry.npmjs.org_fresh___fresh_0.5.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fresh___fresh_0.5.2.tgz";
        url  = "https://registry.npmjs.org/fresh/-/fresh-0.5.2.tgz";
        sha1 = "3d8cadd90d976569fa835ab1f8e4b23a105605a7";
      };
    }
    {
      name = "https___registry.npmjs.org_fs.realpath___fs.realpath_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fs.realpath___fs.realpath_1.0.0.tgz";
        url  = "https://registry.npmjs.org/fs.realpath/-/fs.realpath-1.0.0.tgz";
        sha1 = "1504ad2523158caa40db4a2787cb01411994ea4f";
      };
    }
    {
      name = "https___registry.npmjs.org_fsevents___fsevents_1.2.13.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fsevents___fsevents_1.2.13.tgz";
        url  = "https://registry.npmjs.org/fsevents/-/fsevents-1.2.13.tgz";
        sha1 = "f325cb0455592428bcf11b383370ef70e3bfcc38";
      };
    }
    {
      name = "https___registry.npmjs.org_fsevents___fsevents_2.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fsevents___fsevents_2.2.1.tgz";
        url  = "https://registry.npmjs.org/fsevents/-/fsevents-2.2.1.tgz";
        sha1 = "1fb02ded2036a8ac288d507a65962bd87b97628d";
      };
    }
    {
      name = "https___registry.npmjs.org_function_bind___function_bind_1.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_function_bind___function_bind_1.1.1.tgz";
        url  = "https://registry.npmjs.org/function-bind/-/function-bind-1.1.1.tgz";
        sha1 = "a56899d3ea3c9bab874bb9773b7c5ede92f4895d";
      };
    }
    {
      name = "https___registry.npmjs.org_gensync___gensync_1.0.0_beta.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_gensync___gensync_1.0.0_beta.2.tgz";
        url  = "https://registry.npmjs.org/gensync/-/gensync-1.0.0-beta.2.tgz";
        sha1 = "32a6ee76c3d7f52d46b2b1ae5d93fea8580a25e0";
      };
    }
    {
      name = "https___registry.npmjs.org_get_caller_file___get_caller_file_2.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_get_caller_file___get_caller_file_2.0.5.tgz";
        url  = "https://registry.npmjs.org/get-caller-file/-/get-caller-file-2.0.5.tgz";
        sha1 = "4f94412a82db32f36e3b0b9741f8a97feb031f7e";
      };
    }
    {
      name = "https___registry.npmjs.org_get_intrinsic___get_intrinsic_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_get_intrinsic___get_intrinsic_1.0.1.tgz";
        url  = "https://registry.npmjs.org/get-intrinsic/-/get-intrinsic-1.0.1.tgz";
        sha1 = "94a9768fcbdd0595a1c9273aacf4c89d075631be";
      };
    }
    {
      name = "https___registry.npmjs.org_get_package_type___get_package_type_0.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_get_package_type___get_package_type_0.1.0.tgz";
        url  = "https://registry.npmjs.org/get-package-type/-/get-package-type-0.1.0.tgz";
        sha1 = "8de2d803cff44df3bc6c456e6668b36c3926e11a";
      };
    }
    {
      name = "https___registry.npmjs.org_get_port___get_port_3.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_get_port___get_port_3.2.0.tgz";
        url  = "https://registry.npmjs.org/get-port/-/get-port-3.2.0.tgz";
        sha1 = "dd7ce7de187c06c8bf353796ac71e099f0980ebc";
      };
    }
    {
      name = "https___registry.npmjs.org_get_stream___get_stream_4.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_get_stream___get_stream_4.1.0.tgz";
        url  = "https://registry.npmjs.org/get-stream/-/get-stream-4.1.0.tgz";
        sha1 = "c1b255575f3dc21d59bfc79cd3d2b46b1c3a54b5";
      };
    }
    {
      name = "https___registry.npmjs.org_get_stream___get_stream_5.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_get_stream___get_stream_5.2.0.tgz";
        url  = "https://registry.npmjs.org/get-stream/-/get-stream-5.2.0.tgz";
        sha1 = "4966a1795ee5ace65e706c4b7beb71257d6e22d3";
      };
    }
    {
      name = "https___registry.npmjs.org_get_value___get_value_2.0.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_get_value___get_value_2.0.6.tgz";
        url  = "https://registry.npmjs.org/get-value/-/get-value-2.0.6.tgz";
        sha1 = "dc15ca1c672387ca76bd37ac0a395ba2042a2c28";
      };
    }
    {
      name = "https___registry.npmjs.org_getpass___getpass_0.1.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_getpass___getpass_0.1.7.tgz";
        url  = "https://registry.npmjs.org/getpass/-/getpass-0.1.7.tgz";
        sha1 = "5eff8e3e684d569ae4cb2b1282604e8ba62149fa";
      };
    }
    {
      name = "https___registry.npmjs.org_glob_parent___glob_parent_3.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_glob_parent___glob_parent_3.1.0.tgz";
        url  = "https://registry.npmjs.org/glob-parent/-/glob-parent-3.1.0.tgz";
        sha1 = "9e6af6299d8d3bd2bd40430832bd113df906c5ae";
      };
    }
    {
      name = "https___registry.npmjs.org_glob_to_regexp___glob_to_regexp_0.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_glob_to_regexp___glob_to_regexp_0.3.0.tgz";
        url  = "https://registry.npmjs.org/glob-to-regexp/-/glob-to-regexp-0.3.0.tgz";
        sha1 = "8c5a1494d2066c570cc3bfe4496175acc4d502ab";
      };
    }
    {
      name = "https___registry.npmjs.org_glob___glob_7.1.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_glob___glob_7.1.6.tgz";
        url  = "https://registry.npmjs.org/glob/-/glob-7.1.6.tgz";
        sha1 = "141f33b81a7c2492e125594307480c46679278a6";
      };
    }
    {
      name = "https___registry.npmjs.org_globals___globals_11.12.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_globals___globals_11.12.0.tgz";
        url  = "https://registry.npmjs.org/globals/-/globals-11.12.0.tgz";
        sha1 = "ab8795338868a0babd8525758018c2a7eb95c42e";
      };
    }
    {
      name = "https___registry.npmjs.org_graceful_fs___graceful_fs_4.2.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_graceful_fs___graceful_fs_4.2.4.tgz";
        url  = "https://registry.npmjs.org/graceful-fs/-/graceful-fs-4.2.4.tgz";
        sha1 = "2256bde14d3632958c465ebc96dc467ca07a29fb";
      };
    }
    {
      name = "https___registry.npmjs.org_grapheme_breaker___grapheme_breaker_0.3.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_grapheme_breaker___grapheme_breaker_0.3.2.tgz";
        url  = "https://registry.npmjs.org/grapheme-breaker/-/grapheme-breaker-0.3.2.tgz";
        sha1 = "5b9e6b78c3832452d2ba2bb1cb830f96276410ac";
      };
    }
    {
      name = "https___registry.npmjs.org_growly___growly_1.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_growly___growly_1.3.0.tgz";
        url  = "https://registry.npmjs.org/growly/-/growly-1.3.0.tgz";
        sha1 = "f10748cbe76af964b7c96c93c6bcc28af120c081";
      };
    }
    {
      name = "https___registry.npmjs.org_har_schema___har_schema_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_har_schema___har_schema_2.0.0.tgz";
        url  = "https://registry.npmjs.org/har-schema/-/har-schema-2.0.0.tgz";
        sha1 = "a94c2224ebcac04782a0d9035521f24735b7ec92";
      };
    }
    {
      name = "https___registry.npmjs.org_har_validator___har_validator_5.1.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_har_validator___har_validator_5.1.5.tgz";
        url  = "https://registry.npmjs.org/har-validator/-/har-validator-5.1.5.tgz";
        sha1 = "1f0803b9f8cb20c0fa13822df1ecddb36bde1efd";
      };
    }
    {
      name = "https___registry.npmjs.org_has_ansi___has_ansi_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_has_ansi___has_ansi_2.0.0.tgz";
        url  = "https://registry.npmjs.org/has-ansi/-/has-ansi-2.0.0.tgz";
        sha1 = "34f5049ce1ecdf2b0649af3ef24e45ed35416d91";
      };
    }
    {
      name = "https___registry.npmjs.org_has_flag___has_flag_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_has_flag___has_flag_1.0.0.tgz";
        url  = "https://registry.npmjs.org/has-flag/-/has-flag-1.0.0.tgz";
        sha1 = "9d9e793165ce017a00f00418c43f942a7b1d11fa";
      };
    }
    {
      name = "https___registry.npmjs.org_has_flag___has_flag_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_has_flag___has_flag_3.0.0.tgz";
        url  = "https://registry.npmjs.org/has-flag/-/has-flag-3.0.0.tgz";
        sha1 = "b5d454dc2199ae225699f3467e5a07f3b955bafd";
      };
    }
    {
      name = "https___registry.npmjs.org_has_flag___has_flag_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_has_flag___has_flag_4.0.0.tgz";
        url  = "https://registry.npmjs.org/has-flag/-/has-flag-4.0.0.tgz";
        sha1 = "944771fd9c81c81265c4d6941860da06bb59479b";
      };
    }
    {
      name = "https___registry.npmjs.org_has_symbols___has_symbols_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_has_symbols___has_symbols_1.0.1.tgz";
        url  = "https://registry.npmjs.org/has-symbols/-/has-symbols-1.0.1.tgz";
        sha1 = "9f5214758a44196c406d9bd76cebf81ec2dd31e8";
      };
    }
    {
      name = "https___registry.npmjs.org_has_value___has_value_0.3.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_has_value___has_value_0.3.1.tgz";
        url  = "https://registry.npmjs.org/has-value/-/has-value-0.3.1.tgz";
        sha1 = "7b1f58bada62ca827ec0a2078025654845995e1f";
      };
    }
    {
      name = "https___registry.npmjs.org_has_value___has_value_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_has_value___has_value_1.0.0.tgz";
        url  = "https://registry.npmjs.org/has-value/-/has-value-1.0.0.tgz";
        sha1 = "18b281da585b1c5c51def24c930ed29a0be6b177";
      };
    }
    {
      name = "https___registry.npmjs.org_has_values___has_values_0.1.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_has_values___has_values_0.1.4.tgz";
        url  = "https://registry.npmjs.org/has-values/-/has-values-0.1.4.tgz";
        sha1 = "6d61de95d91dfca9b9a02089ad384bff8f62b771";
      };
    }
    {
      name = "https___registry.npmjs.org_has_values___has_values_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_has_values___has_values_1.0.0.tgz";
        url  = "https://registry.npmjs.org/has-values/-/has-values-1.0.0.tgz";
        sha1 = "95b0b63fec2146619a6fe57fe75628d5a39efe4f";
      };
    }
    {
      name = "https___registry.npmjs.org_has___has_1.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_has___has_1.0.3.tgz";
        url  = "https://registry.npmjs.org/has/-/has-1.0.3.tgz";
        sha1 = "722d7cbfc1f6aa8241f16dd814e011e1f41e8796";
      };
    }
    {
      name = "https___registry.npmjs.org_hash_base___hash_base_3.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_hash_base___hash_base_3.1.0.tgz";
        url  = "https://registry.npmjs.org/hash-base/-/hash-base-3.1.0.tgz";
        sha1 = "55c381d9e06e1d2997a883b4a3fddfe7f0d3af33";
      };
    }
    {
      name = "https___registry.npmjs.org_hash.js___hash.js_1.1.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_hash.js___hash.js_1.1.7.tgz";
        url  = "https://registry.npmjs.org/hash.js/-/hash.js-1.1.7.tgz";
        sha1 = "0babca538e8d4ee4a0f8988d68866537a003cf42";
      };
    }
    {
      name = "https___registry.npmjs.org_hex_color_regex___hex_color_regex_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_hex_color_regex___hex_color_regex_1.1.0.tgz";
        url  = "https://registry.npmjs.org/hex-color-regex/-/hex-color-regex-1.1.0.tgz";
        sha1 = "4c06fccb4602fe2602b3c93df82d7e7dbf1a8a8e";
      };
    }
    {
      name = "https___registry.npmjs.org_hmac_drbg___hmac_drbg_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_hmac_drbg___hmac_drbg_1.0.1.tgz";
        url  = "https://registry.npmjs.org/hmac-drbg/-/hmac-drbg-1.0.1.tgz";
        sha1 = "d2745701025a6c775a6c545793ed502fc0c649a1";
      };
    }
    {
      name = "https___registry.npmjs.org_hosted_git_info___hosted_git_info_2.8.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_hosted_git_info___hosted_git_info_2.8.8.tgz";
        url  = "https://registry.npmjs.org/hosted-git-info/-/hosted-git-info-2.8.8.tgz";
        sha1 = "7539bd4bc1e0e0a895815a2e0262420b12858488";
      };
    }
    {
      name = "https___registry.npmjs.org_hsl_regex___hsl_regex_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_hsl_regex___hsl_regex_1.0.0.tgz";
        url  = "https://registry.npmjs.org/hsl-regex/-/hsl-regex-1.0.0.tgz";
        sha1 = "d49330c789ed819e276a4c0d272dffa30b18fe6e";
      };
    }
    {
      name = "https___registry.npmjs.org_hsla_regex___hsla_regex_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_hsla_regex___hsla_regex_1.0.0.tgz";
        url  = "https://registry.npmjs.org/hsla-regex/-/hsla-regex-1.0.0.tgz";
        sha1 = "c1ce7a3168c8c6614033a4b5f7877f3b225f9c38";
      };
    }
    {
      name = "https___registry.npmjs.org_html_comment_regex___html_comment_regex_1.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_html_comment_regex___html_comment_regex_1.1.2.tgz";
        url  = "https://registry.npmjs.org/html-comment-regex/-/html-comment-regex-1.1.2.tgz";
        sha1 = "97d4688aeb5c81886a364faa0cad1dda14d433a7";
      };
    }
    {
      name = "https___registry.npmjs.org_html_encoding_sniffer___html_encoding_sniffer_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_html_encoding_sniffer___html_encoding_sniffer_1.0.2.tgz";
        url  = "https://registry.npmjs.org/html-encoding-sniffer/-/html-encoding-sniffer-1.0.2.tgz";
        sha1 = "e70d84b94da53aa375e11fe3a351be6642ca46f8";
      };
    }
    {
      name = "https___registry.npmjs.org_html_escaper___html_escaper_2.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_html_escaper___html_escaper_2.0.2.tgz";
        url  = "https://registry.npmjs.org/html-escaper/-/html-escaper-2.0.2.tgz";
        sha1 = "dfd60027da36a36dfcbe236262c00a5822681453";
      };
    }
    {
      name = "https___registry.npmjs.org_html_tags___html_tags_1.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_html_tags___html_tags_1.2.0.tgz";
        url  = "https://registry.npmjs.org/html-tags/-/html-tags-1.2.0.tgz";
        sha1 = "c78de65b5663aa597989dd2b7ab49200d7e4db98";
      };
    }
    {
      name = "https___registry.npmjs.org_htmlnano___htmlnano_0.2.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_htmlnano___htmlnano_0.2.8.tgz";
        url  = "https://registry.npmjs.org/htmlnano/-/htmlnano-0.2.8.tgz";
        sha1 = "d9c22daa18c8ea7675a0bf07cc904793ccaeb56f";
      };
    }
    {
      name = "https___registry.npmjs.org_htmlparser2___htmlparser2_3.10.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_htmlparser2___htmlparser2_3.10.1.tgz";
        url  = "https://registry.npmjs.org/htmlparser2/-/htmlparser2-3.10.1.tgz";
        sha1 = "bd679dc3f59897b6a34bb10749c855bb53a9392f";
      };
    }
    {
      name = "https___registry.npmjs.org_http_errors___http_errors_1.7.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_http_errors___http_errors_1.7.3.tgz";
        url  = "https://registry.npmjs.org/http-errors/-/http-errors-1.7.3.tgz";
        sha1 = "6c619e4f9c60308c38519498c14fbb10aacebb06";
      };
    }
    {
      name = "https___registry.npmjs.org_http_signature___http_signature_1.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_http_signature___http_signature_1.2.0.tgz";
        url  = "https://registry.npmjs.org/http-signature/-/http-signature-1.2.0.tgz";
        sha1 = "9aecd925114772f3d95b65a60abb8f7c18fbace1";
      };
    }
    {
      name = "https___registry.npmjs.org_https_browserify___https_browserify_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_https_browserify___https_browserify_1.0.0.tgz";
        url  = "https://registry.npmjs.org/https-browserify/-/https-browserify-1.0.0.tgz";
        sha1 = "ec06c10e0a34c0f2faf199f7fd7fc78fffd03c73";
      };
    }
    {
      name = "https___registry.npmjs.org_human_signals___human_signals_1.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_human_signals___human_signals_1.1.1.tgz";
        url  = "https://registry.npmjs.org/human-signals/-/human-signals-1.1.1.tgz";
        sha1 = "c5b1cd14f50aeae09ab6c59fe63ba3395fe4dfa3";
      };
    }
    {
      name = "https___registry.npmjs.org_iconv_lite___iconv_lite_0.4.24.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_iconv_lite___iconv_lite_0.4.24.tgz";
        url  = "https://registry.npmjs.org/iconv-lite/-/iconv-lite-0.4.24.tgz";
        sha1 = "2022b4b25fbddc21d2f524974a474aafe733908b";
      };
    }
    {
      name = "https___registry.npmjs.org_icss_replace_symbols___icss_replace_symbols_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_icss_replace_symbols___icss_replace_symbols_1.1.0.tgz";
        url  = "https://registry.npmjs.org/icss-replace-symbols/-/icss-replace-symbols-1.1.0.tgz";
        sha1 = "06ea6f83679a7749e386cfe1fe812ae5db223ded";
      };
    }
    {
      name = "https___registry.npmjs.org_ieee754___ieee754_1.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ieee754___ieee754_1.2.1.tgz";
        url  = "https://registry.npmjs.org/ieee754/-/ieee754-1.2.1.tgz";
        sha1 = "8eb7a10a63fff25d15a57b001586d177d1b0d352";
      };
    }
    {
      name = "https___registry.npmjs.org_import_fresh___import_fresh_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_import_fresh___import_fresh_2.0.0.tgz";
        url  = "https://registry.npmjs.org/import-fresh/-/import-fresh-2.0.0.tgz";
        sha1 = "d81355c15612d386c61f9ddd3922d4304822a546";
      };
    }
    {
      name = "https___registry.npmjs.org_import_local___import_local_3.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_import_local___import_local_3.0.2.tgz";
        url  = "https://registry.npmjs.org/import-local/-/import-local-3.0.2.tgz";
        sha1 = "a8cfd0431d1de4a2199703d003e3e62364fa6db6";
      };
    }
    {
      name = "https___registry.npmjs.org_imurmurhash___imurmurhash_0.1.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_imurmurhash___imurmurhash_0.1.4.tgz";
        url  = "https://registry.npmjs.org/imurmurhash/-/imurmurhash-0.1.4.tgz";
        sha1 = "9218b9b2b928a238b13dc4fb6b6d576f231453ea";
      };
    }
    {
      name = "https___registry.npmjs.org_indexes_of___indexes_of_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_indexes_of___indexes_of_1.0.1.tgz";
        url  = "https://registry.npmjs.org/indexes-of/-/indexes-of-1.0.1.tgz";
        sha1 = "f30f716c8e2bd346c7b67d3df3915566a7c05607";
      };
    }
    {
      name = "https___registry.npmjs.org_inflight___inflight_1.0.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_inflight___inflight_1.0.6.tgz";
        url  = "https://registry.npmjs.org/inflight/-/inflight-1.0.6.tgz";
        sha1 = "49bd6331d7d02d0c09bc910a1075ba8165b56df9";
      };
    }
    {
      name = "https___registry.npmjs.org_inherits___inherits_2.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_inherits___inherits_2.0.4.tgz";
        url  = "https://registry.npmjs.org/inherits/-/inherits-2.0.4.tgz";
        sha1 = "0fa2c64f932917c3433a0ded55363aae37416b7c";
      };
    }
    {
      name = "https___registry.npmjs.org_inherits___inherits_2.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_inherits___inherits_2.0.1.tgz";
        url  = "https://registry.npmjs.org/inherits/-/inherits-2.0.1.tgz";
        sha1 = "b17d08d326b4423e568eff719f91b0b1cbdf69f1";
      };
    }
    {
      name = "https___registry.npmjs.org_inherits___inherits_2.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_inherits___inherits_2.0.3.tgz";
        url  = "https://registry.npmjs.org/inherits/-/inherits-2.0.3.tgz";
        sha1 = "633c2c83e3da42a502f52466022480f4208261de";
      };
    }
    {
      name = "https___registry.npmjs.org_ip_regex___ip_regex_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ip_regex___ip_regex_2.1.0.tgz";
        url  = "https://registry.npmjs.org/ip-regex/-/ip-regex-2.1.0.tgz";
        sha1 = "fa78bf5d2e6913c911ce9f819ee5146bb6d844e9";
      };
    }
    {
      name = "https___registry.npmjs.org_is_absolute_url___is_absolute_url_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_absolute_url___is_absolute_url_2.1.0.tgz";
        url  = "https://registry.npmjs.org/is-absolute-url/-/is-absolute-url-2.1.0.tgz";
        sha1 = "50530dfb84fcc9aa7dbe7852e83a37b93b9f2aa6";
      };
    }
    {
      name = "https___registry.npmjs.org_is_absolute_url___is_absolute_url_3.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_absolute_url___is_absolute_url_3.0.3.tgz";
        url  = "https://registry.npmjs.org/is-absolute-url/-/is-absolute-url-3.0.3.tgz";
        sha1 = "96c6a22b6a23929b11ea0afb1836c36ad4a5d698";
      };
    }
    {
      name = "https___registry.npmjs.org_is_accessor_descriptor___is_accessor_descriptor_0.1.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_accessor_descriptor___is_accessor_descriptor_0.1.6.tgz";
        url  = "https://registry.npmjs.org/is-accessor-descriptor/-/is-accessor-descriptor-0.1.6.tgz";
        sha1 = "a9e12cb3ae8d876727eeef3843f8a0897b5c98d6";
      };
    }
    {
      name = "https___registry.npmjs.org_is_accessor_descriptor___is_accessor_descriptor_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_accessor_descriptor___is_accessor_descriptor_1.0.0.tgz";
        url  = "https://registry.npmjs.org/is-accessor-descriptor/-/is-accessor-descriptor-1.0.0.tgz";
        sha1 = "169c2f6d3df1f992618072365c9b0ea1f6878656";
      };
    }
    {
      name = "https___registry.npmjs.org_is_arrayish___is_arrayish_0.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_arrayish___is_arrayish_0.2.1.tgz";
        url  = "https://registry.npmjs.org/is-arrayish/-/is-arrayish-0.2.1.tgz";
        sha1 = "77c99840527aa8ecb1a8ba697b80645a7a926a9d";
      };
    }
    {
      name = "https___registry.npmjs.org_is_arrayish___is_arrayish_0.3.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_arrayish___is_arrayish_0.3.2.tgz";
        url  = "https://registry.npmjs.org/is-arrayish/-/is-arrayish-0.3.2.tgz";
        sha1 = "4574a2ae56f7ab206896fb431eaeed066fdf8f03";
      };
    }
    {
      name = "https___registry.npmjs.org_is_binary_path___is_binary_path_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_binary_path___is_binary_path_1.0.1.tgz";
        url  = "https://registry.npmjs.org/is-binary-path/-/is-binary-path-1.0.1.tgz";
        sha1 = "75f16642b480f187a711c814161fd3a4a7655898";
      };
    }
    {
      name = "https___registry.npmjs.org_is_buffer___is_buffer_1.1.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_buffer___is_buffer_1.1.6.tgz";
        url  = "https://registry.npmjs.org/is-buffer/-/is-buffer-1.1.6.tgz";
        sha1 = "efaa2ea9daa0d7ab2ea13a97b2b8ad51fefbe8be";
      };
    }
    {
      name = "https___registry.npmjs.org_is_callable___is_callable_1.2.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_callable___is_callable_1.2.2.tgz";
        url  = "https://registry.npmjs.org/is-callable/-/is-callable-1.2.2.tgz";
        sha1 = "c7c6715cd22d4ddb48d3e19970223aceabb080d9";
      };
    }
    {
      name = "https___registry.npmjs.org_is_ci___is_ci_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_ci___is_ci_2.0.0.tgz";
        url  = "https://registry.npmjs.org/is-ci/-/is-ci-2.0.0.tgz";
        sha1 = "6bc6334181810e04b5c22b3d589fdca55026404c";
      };
    }
    {
      name = "https___registry.npmjs.org_is_color_stop___is_color_stop_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_color_stop___is_color_stop_1.1.0.tgz";
        url  = "https://registry.npmjs.org/is-color-stop/-/is-color-stop-1.1.0.tgz";
        sha1 = "cfff471aee4dd5c9e158598fbe12967b5cdad345";
      };
    }
    {
      name = "https___registry.npmjs.org_is_core_module___is_core_module_2.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_core_module___is_core_module_2.2.0.tgz";
        url  = "https://registry.npmjs.org/is-core-module/-/is-core-module-2.2.0.tgz";
        sha1 = "97037ef3d52224d85163f5597b2b63d9afed981a";
      };
    }
    {
      name = "https___registry.npmjs.org_is_data_descriptor___is_data_descriptor_0.1.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_data_descriptor___is_data_descriptor_0.1.4.tgz";
        url  = "https://registry.npmjs.org/is-data-descriptor/-/is-data-descriptor-0.1.4.tgz";
        sha1 = "0b5ee648388e2c860282e793f1856fec3f301b56";
      };
    }
    {
      name = "https___registry.npmjs.org_is_data_descriptor___is_data_descriptor_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_data_descriptor___is_data_descriptor_1.0.0.tgz";
        url  = "https://registry.npmjs.org/is-data-descriptor/-/is-data-descriptor-1.0.0.tgz";
        sha1 = "d84876321d0e7add03990406abbbbd36ba9268c7";
      };
    }
    {
      name = "https___registry.npmjs.org_is_date_object___is_date_object_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_date_object___is_date_object_1.0.2.tgz";
        url  = "https://registry.npmjs.org/is-date-object/-/is-date-object-1.0.2.tgz";
        sha1 = "bda736f2cd8fd06d32844e7743bfa7494c3bfd7e";
      };
    }
    {
      name = "https___registry.npmjs.org_is_descriptor___is_descriptor_0.1.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_descriptor___is_descriptor_0.1.6.tgz";
        url  = "https://registry.npmjs.org/is-descriptor/-/is-descriptor-0.1.6.tgz";
        sha1 = "366d8240dde487ca51823b1ab9f07a10a78251ca";
      };
    }
    {
      name = "https___registry.npmjs.org_is_descriptor___is_descriptor_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_descriptor___is_descriptor_1.0.2.tgz";
        url  = "https://registry.npmjs.org/is-descriptor/-/is-descriptor-1.0.2.tgz";
        sha1 = "3b159746a66604b04f8c81524ba365c5f14d86ec";
      };
    }
    {
      name = "https___registry.npmjs.org_is_directory___is_directory_0.3.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_directory___is_directory_0.3.1.tgz";
        url  = "https://registry.npmjs.org/is-directory/-/is-directory-0.3.1.tgz";
        sha1 = "61339b6f2475fc772fd9c9d83f5c8575dc154ae1";
      };
    }
    {
      name = "https___registry.npmjs.org_is_docker___is_docker_2.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_docker___is_docker_2.1.1.tgz";
        url  = "https://registry.npmjs.org/is-docker/-/is-docker-2.1.1.tgz";
        sha1 = "4125a88e44e450d384e09047ede71adc2d144156";
      };
    }
    {
      name = "https___registry.npmjs.org_is_extendable___is_extendable_0.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_extendable___is_extendable_0.1.1.tgz";
        url  = "https://registry.npmjs.org/is-extendable/-/is-extendable-0.1.1.tgz";
        sha1 = "62b110e289a471418e3ec36a617d472e301dfc89";
      };
    }
    {
      name = "https___registry.npmjs.org_is_extendable___is_extendable_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_extendable___is_extendable_1.0.1.tgz";
        url  = "https://registry.npmjs.org/is-extendable/-/is-extendable-1.0.1.tgz";
        sha1 = "a7470f9e426733d81bd81e1155264e3a3507cab4";
      };
    }
    {
      name = "https___registry.npmjs.org_is_extglob___is_extglob_2.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_extglob___is_extglob_2.1.1.tgz";
        url  = "https://registry.npmjs.org/is-extglob/-/is-extglob-2.1.1.tgz";
        sha1 = "a88c02535791f02ed37c76a1b9ea9773c833f8c2";
      };
    }
    {
      name = "https___registry.npmjs.org_is_fullwidth_code_point___is_fullwidth_code_point_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_fullwidth_code_point___is_fullwidth_code_point_3.0.0.tgz";
        url  = "https://registry.npmjs.org/is-fullwidth-code-point/-/is-fullwidth-code-point-3.0.0.tgz";
        sha1 = "f116f8064fe90b3f7844a38997c0b75051269f1d";
      };
    }
    {
      name = "https___registry.npmjs.org_is_generator_fn___is_generator_fn_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_generator_fn___is_generator_fn_2.1.0.tgz";
        url  = "https://registry.npmjs.org/is-generator-fn/-/is-generator-fn-2.1.0.tgz";
        sha1 = "7d140adc389aaf3011a8f2a2a4cfa6faadffb118";
      };
    }
    {
      name = "https___registry.npmjs.org_is_glob___is_glob_3.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_glob___is_glob_3.1.0.tgz";
        url  = "https://registry.npmjs.org/is-glob/-/is-glob-3.1.0.tgz";
        sha1 = "7ba5ae24217804ac70707b96922567486cc3e84a";
      };
    }
    {
      name = "https___registry.npmjs.org_is_glob___is_glob_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_glob___is_glob_4.0.1.tgz";
        url  = "https://registry.npmjs.org/is-glob/-/is-glob-4.0.1.tgz";
        sha1 = "7567dbe9f2f5e2467bc77ab83c4a29482407a5dc";
      };
    }
    {
      name = "https___registry.npmjs.org_is_html___is_html_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_html___is_html_1.1.0.tgz";
        url  = "https://registry.npmjs.org/is-html/-/is-html-1.1.0.tgz";
        sha1 = "e04f1c18d39485111396f9a0273eab51af218464";
      };
    }
    {
      name = "https___registry.npmjs.org_is_negative_zero___is_negative_zero_2.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_negative_zero___is_negative_zero_2.0.1.tgz";
        url  = "https://registry.npmjs.org/is-negative-zero/-/is-negative-zero-2.0.1.tgz";
        sha1 = "3de746c18dda2319241a53675908d8f766f11c24";
      };
    }
    {
      name = "https___registry.npmjs.org_is_number___is_number_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_number___is_number_3.0.0.tgz";
        url  = "https://registry.npmjs.org/is-number/-/is-number-3.0.0.tgz";
        sha1 = "24fd6201a4782cf50561c810276afc7d12d71195";
      };
    }
    {
      name = "https___registry.npmjs.org_is_number___is_number_7.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_number___is_number_7.0.0.tgz";
        url  = "https://registry.npmjs.org/is-number/-/is-number-7.0.0.tgz";
        sha1 = "7535345b896734d5f80c4d06c50955527a14f12b";
      };
    }
    {
      name = "https___registry.npmjs.org_is_obj___is_obj_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_obj___is_obj_2.0.0.tgz";
        url  = "https://registry.npmjs.org/is-obj/-/is-obj-2.0.0.tgz";
        sha1 = "473fb05d973705e3fd9620545018ca8e22ef4982";
      };
    }
    {
      name = "https___registry.npmjs.org_is_plain_object___is_plain_object_2.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_plain_object___is_plain_object_2.0.4.tgz";
        url  = "https://registry.npmjs.org/is-plain-object/-/is-plain-object-2.0.4.tgz";
        sha1 = "2c163b3fafb1b606d9d17928f05c2a1c38e07677";
      };
    }
    {
      name = "https___registry.npmjs.org_is_regex___is_regex_1.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_regex___is_regex_1.1.1.tgz";
        url  = "https://registry.npmjs.org/is-regex/-/is-regex-1.1.1.tgz";
        sha1 = "c6f98aacc546f6cec5468a07b7b153ab564a57b9";
      };
    }
    {
      name = "https___registry.npmjs.org_is_resolvable___is_resolvable_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_resolvable___is_resolvable_1.1.0.tgz";
        url  = "https://registry.npmjs.org/is-resolvable/-/is-resolvable-1.1.0.tgz";
        sha1 = "fb18f87ce1feb925169c9a407c19318a3206ed88";
      };
    }
    {
      name = "https___registry.npmjs.org_is_stream___is_stream_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_stream___is_stream_1.1.0.tgz";
        url  = "https://registry.npmjs.org/is-stream/-/is-stream-1.1.0.tgz";
        sha1 = "12d4a3dd4e68e0b79ceb8dbc84173ae80d91ca44";
      };
    }
    {
      name = "https___registry.npmjs.org_is_stream___is_stream_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_stream___is_stream_2.0.0.tgz";
        url  = "https://registry.npmjs.org/is-stream/-/is-stream-2.0.0.tgz";
        sha1 = "bde9c32680d6fae04129d6ac9d921ce7815f78e3";
      };
    }
    {
      name = "https___registry.npmjs.org_is_svg___is_svg_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_svg___is_svg_3.0.0.tgz";
        url  = "https://registry.npmjs.org/is-svg/-/is-svg-3.0.0.tgz";
        sha1 = "9321dbd29c212e5ca99c4fa9794c714bcafa2f75";
      };
    }
    {
      name = "https___registry.npmjs.org_is_symbol___is_symbol_1.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_symbol___is_symbol_1.0.3.tgz";
        url  = "https://registry.npmjs.org/is-symbol/-/is-symbol-1.0.3.tgz";
        sha1 = "38e1014b9e6329be0de9d24a414fd7441ec61937";
      };
    }
    {
      name = "https___registry.npmjs.org_is_typedarray___is_typedarray_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_typedarray___is_typedarray_1.0.0.tgz";
        url  = "https://registry.npmjs.org/is-typedarray/-/is-typedarray-1.0.0.tgz";
        sha1 = "e479c80858df0c1b11ddda6940f96011fcda4a9a";
      };
    }
    {
      name = "https___registry.npmjs.org_is_url___is_url_1.2.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_url___is_url_1.2.4.tgz";
        url  = "https://registry.npmjs.org/is-url/-/is-url-1.2.4.tgz";
        sha1 = "04a4df46d28c4cff3d73d01ff06abeb318a1aa52";
      };
    }
    {
      name = "https___registry.npmjs.org_is_windows___is_windows_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_windows___is_windows_1.0.2.tgz";
        url  = "https://registry.npmjs.org/is-windows/-/is-windows-1.0.2.tgz";
        sha1 = "d1850eb9791ecd18e6182ce12a30f396634bb19d";
      };
    }
    {
      name = "https___registry.npmjs.org_is_wsl___is_wsl_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_wsl___is_wsl_1.1.0.tgz";
        url  = "https://registry.npmjs.org/is-wsl/-/is-wsl-1.1.0.tgz";
        sha1 = "1f16e4aa22b04d1336b66188a66af3c600c3a66d";
      };
    }
    {
      name = "https___registry.npmjs.org_is_wsl___is_wsl_2.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_wsl___is_wsl_2.2.0.tgz";
        url  = "https://registry.npmjs.org/is-wsl/-/is-wsl-2.2.0.tgz";
        sha1 = "74a4c76e77ca9fd3f932f290c17ea326cd157271";
      };
    }
    {
      name = "https___registry.npmjs.org_isarray___isarray_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_isarray___isarray_1.0.0.tgz";
        url  = "https://registry.npmjs.org/isarray/-/isarray-1.0.0.tgz";
        sha1 = "bb935d48582cba168c06834957a54a3e07124f11";
      };
    }
    {
      name = "https___registry.npmjs.org_isarray___isarray_2.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_isarray___isarray_2.0.5.tgz";
        url  = "https://registry.npmjs.org/isarray/-/isarray-2.0.5.tgz";
        sha1 = "8af1e4c1221244cc62459faf38940d4e644a5723";
      };
    }
    {
      name = "https___registry.npmjs.org_isexe___isexe_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_isexe___isexe_2.0.0.tgz";
        url  = "https://registry.npmjs.org/isexe/-/isexe-2.0.0.tgz";
        sha1 = "e8fbf374dc556ff8947a10dcb0572d633f2cfa10";
      };
    }
    {
      name = "https___registry.npmjs.org_isobject___isobject_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_isobject___isobject_2.1.0.tgz";
        url  = "https://registry.npmjs.org/isobject/-/isobject-2.1.0.tgz";
        sha1 = "f065561096a3f1da2ef46272f815c840d87e0c89";
      };
    }
    {
      name = "https___registry.npmjs.org_isobject___isobject_3.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_isobject___isobject_3.0.1.tgz";
        url  = "https://registry.npmjs.org/isobject/-/isobject-3.0.1.tgz";
        sha1 = "4e431e92b11a9731636aa1f9c8d1ccbcfdab78df";
      };
    }
    {
      name = "https___registry.npmjs.org_isstream___isstream_0.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_isstream___isstream_0.1.2.tgz";
        url  = "https://registry.npmjs.org/isstream/-/isstream-0.1.2.tgz";
        sha1 = "47e63f7af55afa6f92e1500e690eb8b8529c099a";
      };
    }
    {
      name = "https___registry.npmjs.org_istanbul_lib_coverage___istanbul_lib_coverage_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_istanbul_lib_coverage___istanbul_lib_coverage_3.0.0.tgz";
        url  = "https://registry.npmjs.org/istanbul-lib-coverage/-/istanbul-lib-coverage-3.0.0.tgz";
        sha1 = "f5944a37c70b550b02a78a5c3b2055b280cec8ec";
      };
    }
    {
      name = "https___registry.npmjs.org_istanbul_lib_instrument___istanbul_lib_instrument_4.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_istanbul_lib_instrument___istanbul_lib_instrument_4.0.3.tgz";
        url  = "https://registry.npmjs.org/istanbul-lib-instrument/-/istanbul-lib-instrument-4.0.3.tgz";
        sha1 = "873c6fff897450118222774696a3f28902d77c1d";
      };
    }
    {
      name = "https___registry.npmjs.org_istanbul_lib_report___istanbul_lib_report_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_istanbul_lib_report___istanbul_lib_report_3.0.0.tgz";
        url  = "https://registry.npmjs.org/istanbul-lib-report/-/istanbul-lib-report-3.0.0.tgz";
        sha1 = "7518fe52ea44de372f460a76b5ecda9ffb73d8a6";
      };
    }
    {
      name = "https___registry.npmjs.org_istanbul_lib_source_maps___istanbul_lib_source_maps_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_istanbul_lib_source_maps___istanbul_lib_source_maps_4.0.0.tgz";
        url  = "https://registry.npmjs.org/istanbul-lib-source-maps/-/istanbul-lib-source-maps-4.0.0.tgz";
        sha1 = "75743ce6d96bb86dc7ee4352cf6366a23f0b1ad9";
      };
    }
    {
      name = "https___registry.npmjs.org_istanbul_reports___istanbul_reports_3.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_istanbul_reports___istanbul_reports_3.0.2.tgz";
        url  = "https://registry.npmjs.org/istanbul-reports/-/istanbul-reports-3.0.2.tgz";
        sha1 = "d593210e5000683750cb09fc0644e4b6e27fd53b";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_changed_files___jest_changed_files_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_changed_files___jest_changed_files_25.5.0.tgz";
        url  = "https://registry.npmjs.org/jest-changed-files/-/jest-changed-files-25.5.0.tgz";
        sha1 = "141cc23567ceb3f534526f8614ba39421383634c";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_cli___jest_cli_25.5.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_cli___jest_cli_25.5.4.tgz";
        url  = "https://registry.npmjs.org/jest-cli/-/jest-cli-25.5.4.tgz";
        sha1 = "b9f1a84d1301a92c5c217684cb79840831db9f0d";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_config___jest_config_25.5.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_config___jest_config_25.5.4.tgz";
        url  = "https://registry.npmjs.org/jest-config/-/jest-config-25.5.4.tgz";
        sha1 = "38e2057b3f976ef7309b2b2c8dcd2a708a67f02c";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_diff___jest_diff_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_diff___jest_diff_25.5.0.tgz";
        url  = "https://registry.npmjs.org/jest-diff/-/jest-diff-25.5.0.tgz";
        sha1 = "1dd26ed64f96667c068cef026b677dfa01afcfa9";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_docblock___jest_docblock_25.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_docblock___jest_docblock_25.3.0.tgz";
        url  = "https://registry.npmjs.org/jest-docblock/-/jest-docblock-25.3.0.tgz";
        sha1 = "8b777a27e3477cd77a168c05290c471a575623ef";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_each___jest_each_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_each___jest_each_25.5.0.tgz";
        url  = "https://registry.npmjs.org/jest-each/-/jest-each-25.5.0.tgz";
        sha1 = "0c3c2797e8225cb7bec7e4d249dcd96b934be516";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_environment_jsdom___jest_environment_jsdom_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_environment_jsdom___jest_environment_jsdom_25.5.0.tgz";
        url  = "https://registry.npmjs.org/jest-environment-jsdom/-/jest-environment-jsdom-25.5.0.tgz";
        sha1 = "dcbe4da2ea997707997040ecf6e2560aec4e9834";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_environment_node___jest_environment_node_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_environment_node___jest_environment_node_25.5.0.tgz";
        url  = "https://registry.npmjs.org/jest-environment-node/-/jest-environment-node-25.5.0.tgz";
        sha1 = "0f55270d94804902988e64adca37c6ce0f7d07a1";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_get_type___jest_get_type_25.2.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_get_type___jest_get_type_25.2.6.tgz";
        url  = "https://registry.npmjs.org/jest-get-type/-/jest-get-type-25.2.6.tgz";
        sha1 = "0b0a32fab8908b44d508be81681487dbabb8d877";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_haste_map___jest_haste_map_25.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_haste_map___jest_haste_map_25.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-haste-map/-/jest-haste-map-25.5.1.tgz";
        sha1 = "1df10f716c1d94e60a1ebf7798c9fb3da2620943";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_jasmine2___jest_jasmine2_25.5.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_jasmine2___jest_jasmine2_25.5.4.tgz";
        url  = "https://registry.npmjs.org/jest-jasmine2/-/jest-jasmine2-25.5.4.tgz";
        sha1 = "66ca8b328fb1a3c5364816f8958f6970a8526968";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_leak_detector___jest_leak_detector_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_leak_detector___jest_leak_detector_25.5.0.tgz";
        url  = "https://registry.npmjs.org/jest-leak-detector/-/jest-leak-detector-25.5.0.tgz";
        sha1 = "2291c6294b0ce404241bb56fe60e2d0c3e34f0bb";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_matcher_utils___jest_matcher_utils_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_matcher_utils___jest_matcher_utils_25.5.0.tgz";
        url  = "https://registry.npmjs.org/jest-matcher-utils/-/jest-matcher-utils-25.5.0.tgz";
        sha1 = "fbc98a12d730e5d2453d7f1ed4a4d948e34b7867";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_message_util___jest_message_util_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_message_util___jest_message_util_25.5.0.tgz";
        url  = "https://registry.npmjs.org/jest-message-util/-/jest-message-util-25.5.0.tgz";
        sha1 = "ea11d93204cc7ae97456e1d8716251185b8880ea";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_mock___jest_mock_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_mock___jest_mock_25.5.0.tgz";
        url  = "https://registry.npmjs.org/jest-mock/-/jest-mock-25.5.0.tgz";
        sha1 = "a91a54dabd14e37ecd61665d6b6e06360a55387a";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_pnp_resolver___jest_pnp_resolver_1.2.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_pnp_resolver___jest_pnp_resolver_1.2.2.tgz";
        url  = "https://registry.npmjs.org/jest-pnp-resolver/-/jest-pnp-resolver-1.2.2.tgz";
        sha1 = "b704ac0ae028a89108a4d040b3f919dfddc8e33c";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_regex_util___jest_regex_util_25.2.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_regex_util___jest_regex_util_25.2.6.tgz";
        url  = "https://registry.npmjs.org/jest-regex-util/-/jest-regex-util-25.2.6.tgz";
        sha1 = "d847d38ba15d2118d3b06390056028d0f2fd3964";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_resolve_dependencies___jest_resolve_dependencies_25.5.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_resolve_dependencies___jest_resolve_dependencies_25.5.4.tgz";
        url  = "https://registry.npmjs.org/jest-resolve-dependencies/-/jest-resolve-dependencies-25.5.4.tgz";
        sha1 = "85501f53957c8e3be446e863a74777b5a17397a7";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_resolve___jest_resolve_25.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_resolve___jest_resolve_25.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-resolve/-/jest-resolve-25.5.1.tgz";
        sha1 = "0e6fbcfa7c26d2a5fe8f456088dc332a79266829";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_runner___jest_runner_25.5.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_runner___jest_runner_25.5.4.tgz";
        url  = "https://registry.npmjs.org/jest-runner/-/jest-runner-25.5.4.tgz";
        sha1 = "ffec5df3875da5f5c878ae6d0a17b8e4ecd7c71d";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_runtime___jest_runtime_25.5.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_runtime___jest_runtime_25.5.4.tgz";
        url  = "https://registry.npmjs.org/jest-runtime/-/jest-runtime-25.5.4.tgz";
        sha1 = "dc981fe2cb2137abcd319e74ccae7f7eeffbfaab";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_serializer___jest_serializer_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_serializer___jest_serializer_25.5.0.tgz";
        url  = "https://registry.npmjs.org/jest-serializer/-/jest-serializer-25.5.0.tgz";
        sha1 = "a993f484e769b4ed54e70e0efdb74007f503072b";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_snapshot___jest_snapshot_25.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_snapshot___jest_snapshot_25.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-snapshot/-/jest-snapshot-25.5.1.tgz";
        sha1 = "1a2a576491f9961eb8d00c2e5fd479bc28e5ff7f";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_util___jest_util_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_util___jest_util_25.5.0.tgz";
        url  = "https://registry.npmjs.org/jest-util/-/jest-util-25.5.0.tgz";
        sha1 = "31c63b5d6e901274d264a4fec849230aa3fa35b0";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_validate___jest_validate_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_validate___jest_validate_25.5.0.tgz";
        url  = "https://registry.npmjs.org/jest-validate/-/jest-validate-25.5.0.tgz";
        sha1 = "fb4c93f332c2e4cf70151a628e58a35e459a413a";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_watcher___jest_watcher_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_watcher___jest_watcher_25.5.0.tgz";
        url  = "https://registry.npmjs.org/jest-watcher/-/jest-watcher-25.5.0.tgz";
        sha1 = "d6110d101df98badebe435003956fd4a465e8456";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_worker___jest_worker_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_worker___jest_worker_25.5.0.tgz";
        url  = "https://registry.npmjs.org/jest-worker/-/jest-worker-25.5.0.tgz";
        sha1 = "2611d071b79cea0f43ee57a3d118593ac1547db1";
      };
    }
    {
      name = "https___registry.npmjs.org_jest___jest_25.5.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest___jest_25.5.4.tgz";
        url  = "https://registry.npmjs.org/jest/-/jest-25.5.4.tgz";
        sha1 = "f21107b6489cfe32b076ce2adcadee3587acb9db";
      };
    }
    {
      name = "https___registry.npmjs.org_js_tokens___js_tokens_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_js_tokens___js_tokens_4.0.0.tgz";
        url  = "https://registry.npmjs.org/js-tokens/-/js-tokens-4.0.0.tgz";
        sha1 = "19203fb59991df98e3a287050d4647cdeaf32499";
      };
    }
    {
      name = "https___registry.npmjs.org_js_yaml___js_yaml_3.14.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_js_yaml___js_yaml_3.14.1.tgz";
        url  = "https://registry.npmjs.org/js-yaml/-/js-yaml-3.14.1.tgz";
        sha1 = "dae812fdb3825fa306609a8717383c50c36a0537";
      };
    }
    {
      name = "https___registry.npmjs.org_jsbn___jsbn_0.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jsbn___jsbn_0.1.1.tgz";
        url  = "https://registry.npmjs.org/jsbn/-/jsbn-0.1.1.tgz";
        sha1 = "a5e654c2e5a2deb5f201d96cefbca80c0ef2f513";
      };
    }
    {
      name = "https___registry.npmjs.org_jsdom___jsdom_14.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jsdom___jsdom_14.1.0.tgz";
        url  = "https://registry.npmjs.org/jsdom/-/jsdom-14.1.0.tgz";
        sha1 = "916463b6094956b0a6c1782c94e380cd30e1981b";
      };
    }
    {
      name = "https___registry.npmjs.org_jsdom___jsdom_15.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jsdom___jsdom_15.2.1.tgz";
        url  = "https://registry.npmjs.org/jsdom/-/jsdom-15.2.1.tgz";
        sha1 = "d2feb1aef7183f86be521b8c6833ff5296d07ec5";
      };
    }
    {
      name = "https___registry.npmjs.org_jsesc___jsesc_2.5.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jsesc___jsesc_2.5.2.tgz";
        url  = "https://registry.npmjs.org/jsesc/-/jsesc-2.5.2.tgz";
        sha1 = "80564d2e483dacf6e8ef209650a67df3f0c283a4";
      };
    }
    {
      name = "https___registry.npmjs.org_jsesc___jsesc_0.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jsesc___jsesc_0.5.0.tgz";
        url  = "https://registry.npmjs.org/jsesc/-/jsesc-0.5.0.tgz";
        sha1 = "e7dee66e35d6fc16f710fe91d5cf69f70f08911d";
      };
    }
    {
      name = "https___registry.npmjs.org_json_parse_better_errors___json_parse_better_errors_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_json_parse_better_errors___json_parse_better_errors_1.0.2.tgz";
        url  = "https://registry.npmjs.org/json-parse-better-errors/-/json-parse-better-errors-1.0.2.tgz";
        sha1 = "bb867cfb3450e69107c131d1c514bab3dc8bcaa9";
      };
    }
    {
      name = "https___registry.npmjs.org_json_parse_even_better_errors___json_parse_even_better_errors_2.3.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_json_parse_even_better_errors___json_parse_even_better_errors_2.3.1.tgz";
        url  = "https://registry.npmjs.org/json-parse-even-better-errors/-/json-parse-even-better-errors-2.3.1.tgz";
        sha1 = "7c47805a94319928e05777405dc12e1f7a4ee02d";
      };
    }
    {
      name = "https___registry.npmjs.org_json_schema_traverse___json_schema_traverse_0.4.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_json_schema_traverse___json_schema_traverse_0.4.1.tgz";
        url  = "https://registry.npmjs.org/json-schema-traverse/-/json-schema-traverse-0.4.1.tgz";
        sha1 = "69f6a87d9513ab8bb8fe63bdb0979c448e684660";
      };
    }
    {
      name = "https___registry.npmjs.org_json_schema___json_schema_0.2.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_json_schema___json_schema_0.2.3.tgz";
        url  = "https://registry.npmjs.org/json-schema/-/json-schema-0.2.3.tgz";
        sha1 = "b480c892e59a2f05954ce727bd3f2a4e882f9e13";
      };
    }
    {
      name = "https___registry.npmjs.org_json_stringify_safe___json_stringify_safe_5.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_json_stringify_safe___json_stringify_safe_5.0.1.tgz";
        url  = "https://registry.npmjs.org/json-stringify-safe/-/json-stringify-safe-5.0.1.tgz";
        sha1 = "1296a2d58fd45f19a0f6ce01d65701e2c735b6eb";
      };
    }
    {
      name = "https___registry.npmjs.org_json5___json5_2.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_json5___json5_2.1.3.tgz";
        url  = "https://registry.npmjs.org/json5/-/json5-2.1.3.tgz";
        sha1 = "c9b0f7fa9233bfe5807fe66fcf3a5617ed597d43";
      };
    }
    {
      name = "https___registry.npmjs.org_json5___json5_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_json5___json5_1.0.1.tgz";
        url  = "https://registry.npmjs.org/json5/-/json5-1.0.1.tgz";
        sha1 = "779fb0018604fa854eacbf6252180d83543e3dbe";
      };
    }
    {
      name = "https___registry.npmjs.org_jsprim___jsprim_1.4.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jsprim___jsprim_1.4.1.tgz";
        url  = "https://registry.npmjs.org/jsprim/-/jsprim-1.4.1.tgz";
        sha1 = "313e66bc1e5cc06e438bc1b7499c2e5c56acb6a2";
      };
    }
    {
      name = "https___registry.npmjs.org_kind_of___kind_of_3.2.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_kind_of___kind_of_3.2.2.tgz";
        url  = "https://registry.npmjs.org/kind-of/-/kind-of-3.2.2.tgz";
        sha1 = "31ea21a734bab9bbb0f32466d893aea51e4a3c64";
      };
    }
    {
      name = "https___registry.npmjs.org_kind_of___kind_of_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_kind_of___kind_of_4.0.0.tgz";
        url  = "https://registry.npmjs.org/kind-of/-/kind-of-4.0.0.tgz";
        sha1 = "20813df3d712928b207378691a45066fae72dd57";
      };
    }
    {
      name = "https___registry.npmjs.org_kind_of___kind_of_5.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_kind_of___kind_of_5.1.0.tgz";
        url  = "https://registry.npmjs.org/kind-of/-/kind-of-5.1.0.tgz";
        sha1 = "729c91e2d857b7a419a1f9aa65685c4c33f5845d";
      };
    }
    {
      name = "https___registry.npmjs.org_kind_of___kind_of_6.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_kind_of___kind_of_6.0.3.tgz";
        url  = "https://registry.npmjs.org/kind-of/-/kind-of-6.0.3.tgz";
        sha1 = "07c05034a6c349fa06e24fa35aa76db4580ce4dd";
      };
    }
    {
      name = "https___registry.npmjs.org_kleur___kleur_3.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_kleur___kleur_3.0.3.tgz";
        url  = "https://registry.npmjs.org/kleur/-/kleur-3.0.3.tgz";
        sha1 = "a79c9ecc86ee1ce3fa6206d1216c501f147fc07e";
      };
    }
    {
      name = "https___registry.npmjs.org_leven___leven_3.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_leven___leven_3.1.0.tgz";
        url  = "https://registry.npmjs.org/leven/-/leven-3.1.0.tgz";
        sha1 = "77891de834064cccba82ae7842bb6b14a13ed7f2";
      };
    }
    {
      name = "https___registry.npmjs.org_levn___levn_0.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_levn___levn_0.3.0.tgz";
        url  = "https://registry.npmjs.org/levn/-/levn-0.3.0.tgz";
        sha1 = "3b09924edf9f083c0490fdd4c0bc4421e04764ee";
      };
    }
    {
      name = "https___registry.npmjs.org_lines_and_columns___lines_and_columns_1.1.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_lines_and_columns___lines_and_columns_1.1.6.tgz";
        url  = "https://registry.npmjs.org/lines-and-columns/-/lines-and-columns-1.1.6.tgz";
        sha1 = "1c00c743b433cd0a4e80758f7b64a57440d9ff00";
      };
    }
    {
      name = "https___registry.npmjs.org_locate_path___locate_path_5.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_locate_path___locate_path_5.0.0.tgz";
        url  = "https://registry.npmjs.org/locate-path/-/locate-path-5.0.0.tgz";
        sha1 = "1afba396afd676a6d42504d0a67a3a7eb9f62aa0";
      };
    }
    {
      name = "https___registry.npmjs.org_lodash.clone___lodash.clone_4.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_lodash.clone___lodash.clone_4.5.0.tgz";
        url  = "https://registry.npmjs.org/lodash.clone/-/lodash.clone-4.5.0.tgz";
        sha1 = "195870450f5a13192478df4bc3d23d2dea1907b6";
      };
    }
    {
      name = "https___registry.npmjs.org_lodash.memoize___lodash.memoize_4.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_lodash.memoize___lodash.memoize_4.1.2.tgz";
        url  = "https://registry.npmjs.org/lodash.memoize/-/lodash.memoize-4.1.2.tgz";
        sha1 = "bcc6c49a42a2840ed997f323eada5ecd182e0bfe";
      };
    }
    {
      name = "https___registry.npmjs.org_lodash.sortby___lodash.sortby_4.7.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_lodash.sortby___lodash.sortby_4.7.0.tgz";
        url  = "https://registry.npmjs.org/lodash.sortby/-/lodash.sortby-4.7.0.tgz";
        sha1 = "edd14c824e2cc9c1e0b0a1b42bb5210516a42438";
      };
    }
    {
      name = "https___registry.npmjs.org_lodash.uniq___lodash.uniq_4.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_lodash.uniq___lodash.uniq_4.5.0.tgz";
        url  = "https://registry.npmjs.org/lodash.uniq/-/lodash.uniq-4.5.0.tgz";
        sha1 = "d0225373aeb652adc1bc82e4945339a842754773";
      };
    }
    {
      name = "https___registry.npmjs.org_lodash___lodash_4.17.20.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_lodash___lodash_4.17.20.tgz";
        url  = "https://registry.npmjs.org/lodash/-/lodash-4.17.20.tgz";
        sha1 = "b44a9b6297bcb698f1c51a3545a2b3b368d59c52";
      };
    }
    {
      name = "https___registry.npmjs.org_log_symbols___log_symbols_2.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_log_symbols___log_symbols_2.2.0.tgz";
        url  = "https://registry.npmjs.org/log-symbols/-/log-symbols-2.2.0.tgz";
        sha1 = "5740e1c5d6f0dfda4ad9323b5332107ef6b4c40a";
      };
    }
    {
      name = "https___registry.npmjs.org_lolex___lolex_5.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_lolex___lolex_5.1.2.tgz";
        url  = "https://registry.npmjs.org/lolex/-/lolex-5.1.2.tgz";
        sha1 = "953694d098ce7c07bc5ed6d0e42bc6c0c6d5a367";
      };
    }
    {
      name = "https___registry.npmjs.org_magic_string___magic_string_0.22.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_magic_string___magic_string_0.22.5.tgz";
        url  = "https://registry.npmjs.org/magic-string/-/magic-string-0.22.5.tgz";
        sha1 = "8e9cf5afddf44385c1da5bc2a6a0dbd10b03657e";
      };
    }
    {
      name = "https___registry.npmjs.org_make_dir___make_dir_3.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_make_dir___make_dir_3.1.0.tgz";
        url  = "https://registry.npmjs.org/make-dir/-/make-dir-3.1.0.tgz";
        sha1 = "415e967046b3a7f1d185277d84aa58203726a13f";
      };
    }
    {
      name = "https___registry.npmjs.org_make_error___make_error_1.3.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_make_error___make_error_1.3.6.tgz";
        url  = "https://registry.npmjs.org/make-error/-/make-error-1.3.6.tgz";
        sha1 = "2eb2e37ea9b67c4891f684a1394799af484cf7a2";
      };
    }
    {
      name = "https___registry.npmjs.org_makeerror___makeerror_1.0.11.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_makeerror___makeerror_1.0.11.tgz";
        url  = "https://registry.npmjs.org/makeerror/-/makeerror-1.0.11.tgz";
        sha1 = "e01a5c9109f2af79660e4e8b9587790184f5a96c";
      };
    }
    {
      name = "https___registry.npmjs.org_map_cache___map_cache_0.2.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_map_cache___map_cache_0.2.2.tgz";
        url  = "https://registry.npmjs.org/map-cache/-/map-cache-0.2.2.tgz";
        sha1 = "c32abd0bd6525d9b051645bb4f26ac5dc98a0dbf";
      };
    }
    {
      name = "https___registry.npmjs.org_map_visit___map_visit_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_map_visit___map_visit_1.0.0.tgz";
        url  = "https://registry.npmjs.org/map-visit/-/map-visit-1.0.0.tgz";
        sha1 = "ecdca8f13144e660f1b5bd41f12f3479d98dfb8f";
      };
    }
    {
      name = "https___registry.npmjs.org_md5.js___md5.js_1.3.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_md5.js___md5.js_1.3.5.tgz";
        url  = "https://registry.npmjs.org/md5.js/-/md5.js-1.3.5.tgz";
        sha1 = "b5d07b8e3216e3e27cd728d72f70d1e6a342005f";
      };
    }
    {
      name = "https___registry.npmjs.org_mdn_data___mdn_data_2.0.14.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_mdn_data___mdn_data_2.0.14.tgz";
        url  = "https://registry.npmjs.org/mdn-data/-/mdn-data-2.0.14.tgz";
        sha1 = "7113fc4281917d63ce29b43446f701e68c25ba50";
      };
    }
    {
      name = "https___registry.npmjs.org_mdn_data___mdn_data_2.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_mdn_data___mdn_data_2.0.4.tgz";
        url  = "https://registry.npmjs.org/mdn-data/-/mdn-data-2.0.4.tgz";
        sha1 = "699b3c38ac6f1d728091a64650b65d388502fd5b";
      };
    }
    {
      name = "https___registry.npmjs.org_merge_source_map___merge_source_map_1.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_merge_source_map___merge_source_map_1.0.4.tgz";
        url  = "https://registry.npmjs.org/merge-source-map/-/merge-source-map-1.0.4.tgz";
        sha1 = "a5de46538dae84d4114cc5ea02b4772a6346701f";
      };
    }
    {
      name = "https___registry.npmjs.org_merge_stream___merge_stream_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_merge_stream___merge_stream_2.0.0.tgz";
        url  = "https://registry.npmjs.org/merge-stream/-/merge-stream-2.0.0.tgz";
        sha1 = "52823629a14dd00c9770fb6ad47dc6310f2c1f60";
      };
    }
    {
      name = "https___registry.npmjs.org_merge2___merge2_1.4.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_merge2___merge2_1.4.1.tgz";
        url  = "https://registry.npmjs.org/merge2/-/merge2-1.4.1.tgz";
        sha1 = "4368892f885e907455a6fd7dc55c0c9d404990ae";
      };
    }
    {
      name = "https___registry.npmjs.org_micromatch___micromatch_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_micromatch___micromatch_4.0.2.tgz";
        url  = "https://registry.npmjs.org/micromatch/-/micromatch-4.0.2.tgz";
        sha1 = "4fcb0999bf9fbc2fcbdd212f6d629b9a56c39259";
      };
    }
    {
      name = "https___registry.npmjs.org_micromatch___micromatch_3.1.10.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_micromatch___micromatch_3.1.10.tgz";
        url  = "https://registry.npmjs.org/micromatch/-/micromatch-3.1.10.tgz";
        sha1 = "70859bc95c9840952f359a068a3fc49f9ecfac23";
      };
    }
    {
      name = "https___registry.npmjs.org_miller_rabin___miller_rabin_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_miller_rabin___miller_rabin_4.0.1.tgz";
        url  = "https://registry.npmjs.org/miller-rabin/-/miller-rabin-4.0.1.tgz";
        sha1 = "f080351c865b0dc562a8462966daa53543c78a4d";
      };
    }
    {
      name = "https___registry.npmjs.org_mime_db___mime_db_1.44.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_mime_db___mime_db_1.44.0.tgz";
        url  = "https://registry.npmjs.org/mime-db/-/mime-db-1.44.0.tgz";
        sha1 = "fa11c5eb0aca1334b4233cb4d52f10c5a6272f92";
      };
    }
    {
      name = "https___registry.npmjs.org_mime_types___mime_types_2.1.27.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_mime_types___mime_types_2.1.27.tgz";
        url  = "https://registry.npmjs.org/mime-types/-/mime-types-2.1.27.tgz";
        sha1 = "47949f98e279ea53119f5722e0f34e529bec009f";
      };
    }
    {
      name = "https___registry.npmjs.org_mime___mime_1.6.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_mime___mime_1.6.0.tgz";
        url  = "https://registry.npmjs.org/mime/-/mime-1.6.0.tgz";
        sha1 = "32cd9e5c64553bd58d19a568af452acff04981b1";
      };
    }
    {
      name = "https___registry.npmjs.org_mimic_fn___mimic_fn_1.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_mimic_fn___mimic_fn_1.2.0.tgz";
        url  = "https://registry.npmjs.org/mimic-fn/-/mimic-fn-1.2.0.tgz";
        sha1 = "820c86a39334640e99516928bd03fca88057d022";
      };
    }
    {
      name = "https___registry.npmjs.org_mimic_fn___mimic_fn_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_mimic_fn___mimic_fn_2.1.0.tgz";
        url  = "https://registry.npmjs.org/mimic-fn/-/mimic-fn-2.1.0.tgz";
        sha1 = "7ed2c2ccccaf84d3ffcb7a69b57711fc2083401b";
      };
    }
    {
      name = "https___registry.npmjs.org_minimalistic_assert___minimalistic_assert_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_minimalistic_assert___minimalistic_assert_1.0.1.tgz";
        url  = "https://registry.npmjs.org/minimalistic-assert/-/minimalistic-assert-1.0.1.tgz";
        sha1 = "2e194de044626d4a10e7f7fbc00ce73e83e4d5c7";
      };
    }
    {
      name = "https___registry.npmjs.org_minimalistic_crypto_utils___minimalistic_crypto_utils_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_minimalistic_crypto_utils___minimalistic_crypto_utils_1.0.1.tgz";
        url  = "https://registry.npmjs.org/minimalistic-crypto-utils/-/minimalistic-crypto-utils-1.0.1.tgz";
        sha1 = "f6c00c1c0b082246e5c4d99dfb8c7c083b2b582a";
      };
    }
    {
      name = "https___registry.npmjs.org_minimatch___minimatch_3.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_minimatch___minimatch_3.0.4.tgz";
        url  = "https://registry.npmjs.org/minimatch/-/minimatch-3.0.4.tgz";
        sha1 = "5166e286457f03306064be5497e8dbb0c3d32083";
      };
    }
    {
      name = "https___registry.npmjs.org_minimist___minimist_1.2.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_minimist___minimist_1.2.5.tgz";
        url  = "https://registry.npmjs.org/minimist/-/minimist-1.2.5.tgz";
        sha1 = "67d66014b66a6a8aaa0c083c5fd58df4e4e97602";
      };
    }
    {
      name = "https___registry.npmjs.org_mixin_deep___mixin_deep_1.3.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_mixin_deep___mixin_deep_1.3.2.tgz";
        url  = "https://registry.npmjs.org/mixin-deep/-/mixin-deep-1.3.2.tgz";
        sha1 = "1120b43dc359a785dce65b55b82e257ccf479566";
      };
    }
    {
      name = "https___registry.npmjs.org_mkdirp___mkdirp_0.5.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_mkdirp___mkdirp_0.5.5.tgz";
        url  = "https://registry.npmjs.org/mkdirp/-/mkdirp-0.5.5.tgz";
        sha1 = "d91cefd62d1436ca0f41620e251288d420099def";
      };
    }
    {
      name = "https___registry.npmjs.org_ms___ms_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ms___ms_2.0.0.tgz";
        url  = "https://registry.npmjs.org/ms/-/ms-2.0.0.tgz";
        sha1 = "5608aeadfc00be6c2901df5f9861788de0d597c8";
      };
    }
    {
      name = "https___registry.npmjs.org_ms___ms_2.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ms___ms_2.1.1.tgz";
        url  = "https://registry.npmjs.org/ms/-/ms-2.1.1.tgz";
        sha1 = "30a5864eb3ebb0a66f2ebe6d727af06a09d86e0a";
      };
    }
    {
      name = "https___registry.npmjs.org_ms___ms_2.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ms___ms_2.1.2.tgz";
        url  = "https://registry.npmjs.org/ms/-/ms-2.1.2.tgz";
        sha1 = "d09d1f357b443f493382a8eb3ccd183872ae6009";
      };
    }
    {
      name = "https___registry.npmjs.org_nan___nan_2.14.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_nan___nan_2.14.2.tgz";
        url  = "https://registry.npmjs.org/nan/-/nan-2.14.2.tgz";
        sha1 = "f5376400695168f4cc694ac9393d0c9585eeea19";
      };
    }
    {
      name = "https___registry.npmjs.org_nanomatch___nanomatch_1.2.13.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_nanomatch___nanomatch_1.2.13.tgz";
        url  = "https://registry.npmjs.org/nanomatch/-/nanomatch-1.2.13.tgz";
        sha1 = "b87a8aa4fc0de8fe6be88895b38983ff265bd119";
      };
    }
    {
      name = "https___registry.npmjs.org_natural_compare___natural_compare_1.4.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_natural_compare___natural_compare_1.4.0.tgz";
        url  = "https://registry.npmjs.org/natural-compare/-/natural-compare-1.4.0.tgz";
        sha1 = "4abebfeed7541f2c27acfb29bdbbd15c8d5ba4f7";
      };
    }
    {
      name = "https___registry.npmjs.org_nice_try___nice_try_1.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_nice_try___nice_try_1.0.5.tgz";
        url  = "https://registry.npmjs.org/nice-try/-/nice-try-1.0.5.tgz";
        sha1 = "a3378a7696ce7d223e88fc9b764bd7ef1089e366";
      };
    }
    {
      name = "https___registry.npmjs.org_node_addon_api___node_addon_api_1.7.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_node_addon_api___node_addon_api_1.7.2.tgz";
        url  = "https://registry.npmjs.org/node-addon-api/-/node-addon-api-1.7.2.tgz";
        sha1 = "3df30b95720b53c24e59948b49532b662444f54d";
      };
    }
    {
      name = "https___registry.npmjs.org_node_forge___node_forge_0.7.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_node_forge___node_forge_0.7.6.tgz";
        url  = "https://registry.npmjs.org/node-forge/-/node-forge-0.7.6.tgz";
        sha1 = "fdf3b418aee1f94f0ef642cd63486c77ca9724ac";
      };
    }
    {
      name = "https___registry.npmjs.org_node_int64___node_int64_0.4.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_node_int64___node_int64_0.4.0.tgz";
        url  = "https://registry.npmjs.org/node-int64/-/node-int64-0.4.0.tgz";
        sha1 = "87a9065cdb355d3182d8f94ce11188b825c68a3b";
      };
    }
    {
      name = "https___registry.npmjs.org_node_libs_browser___node_libs_browser_2.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_node_libs_browser___node_libs_browser_2.2.1.tgz";
        url  = "https://registry.npmjs.org/node-libs-browser/-/node-libs-browser-2.2.1.tgz";
        sha1 = "b64f513d18338625f90346d27b0d235e631f6425";
      };
    }
    {
      name = "https___registry.npmjs.org_node_modules_regexp___node_modules_regexp_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_node_modules_regexp___node_modules_regexp_1.0.0.tgz";
        url  = "https://registry.npmjs.org/node-modules-regexp/-/node-modules-regexp-1.0.0.tgz";
        sha1 = "8d9dbe28964a4ac5712e9131642107c71e90ec40";
      };
    }
    {
      name = "https___registry.npmjs.org_node_notifier___node_notifier_6.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_node_notifier___node_notifier_6.0.0.tgz";
        url  = "https://registry.npmjs.org/node-notifier/-/node-notifier-6.0.0.tgz";
        sha1 = "cea319e06baa16deec8ce5cd7f133c4a46b68e12";
      };
    }
    {
      name = "https___registry.npmjs.org_node_releases___node_releases_1.1.67.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_node_releases___node_releases_1.1.67.tgz";
        url  = "https://registry.npmjs.org/node-releases/-/node-releases-1.1.67.tgz";
        sha1 = "28ebfcccd0baa6aad8e8d4d8fe4cbc49ae239c12";
      };
    }
    {
      name = "https___registry.npmjs.org_normalize_package_data___normalize_package_data_2.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_normalize_package_data___normalize_package_data_2.5.0.tgz";
        url  = "https://registry.npmjs.org/normalize-package-data/-/normalize-package-data-2.5.0.tgz";
        sha1 = "e66db1838b200c1dfc233225d12cb36520e234a8";
      };
    }
    {
      name = "https___registry.npmjs.org_normalize_path___normalize_path_2.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_normalize_path___normalize_path_2.1.1.tgz";
        url  = "https://registry.npmjs.org/normalize-path/-/normalize-path-2.1.1.tgz";
        sha1 = "1ab28b556e198363a8c1a6f7e6fa20137fe6aed9";
      };
    }
    {
      name = "https___registry.npmjs.org_normalize_path___normalize_path_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_normalize_path___normalize_path_3.0.0.tgz";
        url  = "https://registry.npmjs.org/normalize-path/-/normalize-path-3.0.0.tgz";
        sha1 = "0dcd69ff23a1c9b11fd0978316644a0388216a65";
      };
    }
    {
      name = "https___registry.npmjs.org_normalize_url___normalize_url_3.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_normalize_url___normalize_url_3.3.0.tgz";
        url  = "https://registry.npmjs.org/normalize-url/-/normalize-url-3.3.0.tgz";
        sha1 = "b2e1c4dc4f7c6d57743df733a4f5978d18650559";
      };
    }
    {
      name = "https___registry.npmjs.org_npm_run_path___npm_run_path_2.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_npm_run_path___npm_run_path_2.0.2.tgz";
        url  = "https://registry.npmjs.org/npm-run-path/-/npm-run-path-2.0.2.tgz";
        sha1 = "35a9232dfa35d7067b4cb2ddf2357b1871536c5f";
      };
    }
    {
      name = "https___registry.npmjs.org_npm_run_path___npm_run_path_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_npm_run_path___npm_run_path_4.0.1.tgz";
        url  = "https://registry.npmjs.org/npm-run-path/-/npm-run-path-4.0.1.tgz";
        sha1 = "b7ecd1e5ed53da8e37a55e1c2269e0b97ed748ea";
      };
    }
    {
      name = "https___registry.npmjs.org_nth_check___nth_check_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_nth_check___nth_check_1.0.2.tgz";
        url  = "https://registry.npmjs.org/nth-check/-/nth-check-1.0.2.tgz";
        sha1 = "b2bd295c37e3dd58a3bf0700376663ba4d9cf05c";
      };
    }
    {
      name = "https___registry.npmjs.org_nwsapi___nwsapi_2.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_nwsapi___nwsapi_2.2.0.tgz";
        url  = "https://registry.npmjs.org/nwsapi/-/nwsapi-2.2.0.tgz";
        sha1 = "204879a9e3d068ff2a55139c2c772780681a38b7";
      };
    }
    {
      name = "https___registry.npmjs.org_oauth_sign___oauth_sign_0.9.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_oauth_sign___oauth_sign_0.9.0.tgz";
        url  = "https://registry.npmjs.org/oauth-sign/-/oauth-sign-0.9.0.tgz";
        sha1 = "47a7b016baa68b5fa0ecf3dee08a85c679ac6455";
      };
    }
    {
      name = "https___registry.npmjs.org_object_assign___object_assign_4.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_object_assign___object_assign_4.1.1.tgz";
        url  = "https://registry.npmjs.org/object-assign/-/object-assign-4.1.1.tgz";
        sha1 = "2109adc7965887cfc05cbbd442cac8bfbb360863";
      };
    }
    {
      name = "https___registry.npmjs.org_object_copy___object_copy_0.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_object_copy___object_copy_0.1.0.tgz";
        url  = "https://registry.npmjs.org/object-copy/-/object-copy-0.1.0.tgz";
        sha1 = "7e7d858b781bd7c991a41ba975ed3812754e998c";
      };
    }
    {
      name = "https___registry.npmjs.org_object_inspect___object_inspect_1.9.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_object_inspect___object_inspect_1.9.0.tgz";
        url  = "https://registry.npmjs.org/object-inspect/-/object-inspect-1.9.0.tgz";
        sha1 = "c90521d74e1127b67266ded3394ad6116986533a";
      };
    }
    {
      name = "https___registry.npmjs.org_object_inspect___object_inspect_1.4.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_object_inspect___object_inspect_1.4.1.tgz";
        url  = "https://registry.npmjs.org/object-inspect/-/object-inspect-1.4.1.tgz";
        sha1 = "37ffb10e71adaf3748d05f713b4c9452f402cbc4";
      };
    }
    {
      name = "https___registry.npmjs.org_object_keys___object_keys_1.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_object_keys___object_keys_1.1.1.tgz";
        url  = "https://registry.npmjs.org/object-keys/-/object-keys-1.1.1.tgz";
        sha1 = "1c47f272df277f3b1daf061677d9c82e2322c60e";
      };
    }
    {
      name = "https___registry.npmjs.org_object_visit___object_visit_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_object_visit___object_visit_1.0.1.tgz";
        url  = "https://registry.npmjs.org/object-visit/-/object-visit-1.0.1.tgz";
        sha1 = "f79c4493af0c5377b59fe39d395e41042dd045bb";
      };
    }
    {
      name = "https___registry.npmjs.org_object.assign___object.assign_4.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_object.assign___object.assign_4.1.2.tgz";
        url  = "https://registry.npmjs.org/object.assign/-/object.assign-4.1.2.tgz";
        sha1 = "0ed54a342eceb37b38ff76eb831a0e788cb63940";
      };
    }
    {
      name = "https___registry.npmjs.org_object.getownpropertydescriptors___object.getownpropertydescriptors_2.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_object.getownpropertydescriptors___object.getownpropertydescriptors_2.1.1.tgz";
        url  = "https://registry.npmjs.org/object.getownpropertydescriptors/-/object.getownpropertydescriptors-2.1.1.tgz";
        sha1 = "0dfda8d108074d9c563e80490c883b6661091544";
      };
    }
    {
      name = "https___registry.npmjs.org_object.pick___object.pick_1.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_object.pick___object.pick_1.3.0.tgz";
        url  = "https://registry.npmjs.org/object.pick/-/object.pick-1.3.0.tgz";
        sha1 = "87a10ac4c1694bd2e1cbf53591a66141fb5dd747";
      };
    }
    {
      name = "https___registry.npmjs.org_object.values___object.values_1.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_object.values___object.values_1.1.2.tgz";
        url  = "https://registry.npmjs.org/object.values/-/object.values-1.1.2.tgz";
        sha1 = "7a2015e06fcb0f546bd652486ce8583a4731c731";
      };
    }
    {
      name = "https___registry.npmjs.org_on_finished___on_finished_2.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_on_finished___on_finished_2.3.0.tgz";
        url  = "https://registry.npmjs.org/on-finished/-/on-finished-2.3.0.tgz";
        sha1 = "20f1336481b083cd75337992a16971aa2d906947";
      };
    }
    {
      name = "https___registry.npmjs.org_once___once_1.4.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_once___once_1.4.0.tgz";
        url  = "https://registry.npmjs.org/once/-/once-1.4.0.tgz";
        sha1 = "583b1aa775961d4b113ac17d9c50baef9dd76bd1";
      };
    }
    {
      name = "https___registry.npmjs.org_onetime___onetime_2.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_onetime___onetime_2.0.1.tgz";
        url  = "https://registry.npmjs.org/onetime/-/onetime-2.0.1.tgz";
        sha1 = "067428230fd67443b2794b22bba528b6867962d4";
      };
    }
    {
      name = "https___registry.npmjs.org_onetime___onetime_5.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_onetime___onetime_5.1.2.tgz";
        url  = "https://registry.npmjs.org/onetime/-/onetime-5.1.2.tgz";
        sha1 = "d0e96ebb56b07476df1dd9c4806e5237985ca45e";
      };
    }
    {
      name = "https___registry.npmjs.org_opn___opn_5.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_opn___opn_5.5.0.tgz";
        url  = "https://registry.npmjs.org/opn/-/opn-5.5.0.tgz";
        sha1 = "fc7164fab56d235904c51c3b27da6758ca3b9bfc";
      };
    }
    {
      name = "https___registry.npmjs.org_optionator___optionator_0.8.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_optionator___optionator_0.8.3.tgz";
        url  = "https://registry.npmjs.org/optionator/-/optionator-0.8.3.tgz";
        sha1 = "84fa1d036fe9d3c7e21d99884b601167ec8fb495";
      };
    }
    {
      name = "https___registry.npmjs.org_ora___ora_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ora___ora_2.1.0.tgz";
        url  = "https://registry.npmjs.org/ora/-/ora-2.1.0.tgz";
        sha1 = "6caf2830eb924941861ec53a173799e008b51e5b";
      };
    }
    {
      name = "https___registry.npmjs.org_os_browserify___os_browserify_0.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_os_browserify___os_browserify_0.3.0.tgz";
        url  = "https://registry.npmjs.org/os-browserify/-/os-browserify-0.3.0.tgz";
        sha1 = "854373c7f5c2315914fc9bfc6bd8238fdda1ec27";
      };
    }
    {
      name = "https___registry.npmjs.org_p_each_series___p_each_series_2.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_p_each_series___p_each_series_2.2.0.tgz";
        url  = "https://registry.npmjs.org/p-each-series/-/p-each-series-2.2.0.tgz";
        sha1 = "105ab0357ce72b202a8a8b94933672657b5e2a9a";
      };
    }
    {
      name = "https___registry.npmjs.org_p_finally___p_finally_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_p_finally___p_finally_1.0.0.tgz";
        url  = "https://registry.npmjs.org/p-finally/-/p-finally-1.0.0.tgz";
        sha1 = "3fbcfb15b899a44123b34b6dcc18b724336a2cae";
      };
    }
    {
      name = "https___registry.npmjs.org_p_finally___p_finally_2.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_p_finally___p_finally_2.0.1.tgz";
        url  = "https://registry.npmjs.org/p-finally/-/p-finally-2.0.1.tgz";
        sha1 = "bd6fcaa9c559a096b680806f4d657b3f0f240561";
      };
    }
    {
      name = "https___registry.npmjs.org_p_limit___p_limit_2.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_p_limit___p_limit_2.3.0.tgz";
        url  = "https://registry.npmjs.org/p-limit/-/p-limit-2.3.0.tgz";
        sha1 = "3dd33c647a214fdfffd835933eb086da0dc21db1";
      };
    }
    {
      name = "https___registry.npmjs.org_p_locate___p_locate_4.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_p_locate___p_locate_4.1.0.tgz";
        url  = "https://registry.npmjs.org/p-locate/-/p-locate-4.1.0.tgz";
        sha1 = "a3428bb7088b3a60292f66919278b7c297ad4f07";
      };
    }
    {
      name = "https___registry.npmjs.org_p_try___p_try_2.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_p_try___p_try_2.2.0.tgz";
        url  = "https://registry.npmjs.org/p-try/-/p-try-2.2.0.tgz";
        sha1 = "cb2868540e313d61de58fafbe35ce9004d5540e6";
      };
    }
    {
      name = "https___registry.npmjs.org_pako___pako_0.2.9.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_pako___pako_0.2.9.tgz";
        url  = "https://registry.npmjs.org/pako/-/pako-0.2.9.tgz";
        sha1 = "f3f7522f4ef782348da8161bad9ecfd51bf83a75";
      };
    }
    {
      name = "https___registry.npmjs.org_pako___pako_1.0.11.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_pako___pako_1.0.11.tgz";
        url  = "https://registry.npmjs.org/pako/-/pako-1.0.11.tgz";
        sha1 = "6c9599d340d54dfd3946380252a35705a6b992bf";
      };
    }
    {
      name = "https___registry.npmjs.org_parcel_bundler___parcel_bundler_1.12.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_parcel_bundler___parcel_bundler_1.12.4.tgz";
        url  = "https://registry.npmjs.org/parcel-bundler/-/parcel-bundler-1.12.4.tgz";
        sha1 = "31223f4ab4d00323a109fce28d5e46775409a9ee";
      };
    }
    {
      name = "parcel_plugin_static_files_copy___parcel_plugin_static_files_copy_2.5.0.tgz";
      path = fetchurl {
        name = "parcel_plugin_static_files_copy___parcel_plugin_static_files_copy_2.5.0.tgz";
        url  = "https://registry.yarnpkg.com/parcel-plugin-static-files-copy/-/parcel-plugin-static-files-copy-2.5.0.tgz";
        sha1 = "8ab26f331d163cd023d283fc0ef5fcd779fe5948";
      };
    }
    {
      name = "https___registry.npmjs.org_parse_asn1___parse_asn1_5.1.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_parse_asn1___parse_asn1_5.1.6.tgz";
        url  = "https://registry.npmjs.org/parse-asn1/-/parse-asn1-5.1.6.tgz";
        sha1 = "385080a3ec13cb62a62d39409cb3e88844cdaed4";
      };
    }
    {
      name = "https___registry.npmjs.org_parse_json___parse_json_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_parse_json___parse_json_4.0.0.tgz";
        url  = "https://registry.npmjs.org/parse-json/-/parse-json-4.0.0.tgz";
        sha1 = "be35f5425be1f7f6c747184f98a788cb99477ee0";
      };
    }
    {
      name = "https___registry.npmjs.org_parse_json___parse_json_5.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_parse_json___parse_json_5.1.0.tgz";
        url  = "https://registry.npmjs.org/parse-json/-/parse-json-5.1.0.tgz";
        sha1 = "f96088cdf24a8faa9aea9a009f2d9d942c999646";
      };
    }
    {
      name = "https___registry.npmjs.org_parse5___parse5_5.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_parse5___parse5_5.1.0.tgz";
        url  = "https://registry.npmjs.org/parse5/-/parse5-5.1.0.tgz";
        sha1 = "c59341c9723f414c452975564c7c00a68d58acd2";
      };
    }
    {
      name = "https___registry.npmjs.org_parseurl___parseurl_1.3.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_parseurl___parseurl_1.3.3.tgz";
        url  = "https://registry.npmjs.org/parseurl/-/parseurl-1.3.3.tgz";
        sha1 = "9da19e7bee8d12dff0513ed5b76957793bc2e8d4";
      };
    }
    {
      name = "https___registry.npmjs.org_pascalcase___pascalcase_0.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_pascalcase___pascalcase_0.1.1.tgz";
        url  = "https://registry.npmjs.org/pascalcase/-/pascalcase-0.1.1.tgz";
        sha1 = "b363e55e8006ca6fe21784d2db22bd15d7917f14";
      };
    }
    {
      name = "https___registry.npmjs.org_path_browserify___path_browserify_0.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_path_browserify___path_browserify_0.0.1.tgz";
        url  = "https://registry.npmjs.org/path-browserify/-/path-browserify-0.0.1.tgz";
        sha1 = "e6c4ddd7ed3aa27c68a20cc4e50e1a4ee83bbc4a";
      };
    }
    {
      name = "https___registry.npmjs.org_path_dirname___path_dirname_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_path_dirname___path_dirname_1.0.2.tgz";
        url  = "https://registry.npmjs.org/path-dirname/-/path-dirname-1.0.2.tgz";
        sha1 = "cc33d24d525e099a5388c0336c6e32b9160609e0";
      };
    }
    {
      name = "https___registry.npmjs.org_path_exists___path_exists_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_path_exists___path_exists_4.0.0.tgz";
        url  = "https://registry.npmjs.org/path-exists/-/path-exists-4.0.0.tgz";
        sha1 = "513bdbe2d3b95d7762e8c1137efa195c6c61b5b3";
      };
    }
    {
      name = "https___registry.npmjs.org_path_is_absolute___path_is_absolute_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_path_is_absolute___path_is_absolute_1.0.1.tgz";
        url  = "https://registry.npmjs.org/path-is-absolute/-/path-is-absolute-1.0.1.tgz";
        sha1 = "174b9268735534ffbc7ace6bf53a5a9e1b5c5f5f";
      };
    }
    {
      name = "https___registry.npmjs.org_path_key___path_key_2.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_path_key___path_key_2.0.1.tgz";
        url  = "https://registry.npmjs.org/path-key/-/path-key-2.0.1.tgz";
        sha1 = "411cadb574c5a140d3a4b1910d40d80cc9f40b40";
      };
    }
    {
      name = "https___registry.npmjs.org_path_key___path_key_3.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_path_key___path_key_3.1.1.tgz";
        url  = "https://registry.npmjs.org/path-key/-/path-key-3.1.1.tgz";
        sha1 = "581f6ade658cbba65a0d3380de7753295054f375";
      };
    }
    {
      name = "https___registry.npmjs.org_path_parse___path_parse_1.0.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_path_parse___path_parse_1.0.6.tgz";
        url  = "https://registry.npmjs.org/path-parse/-/path-parse-1.0.6.tgz";
        sha1 = "d62dbb5679405d72c4737ec58600e9ddcf06d24c";
      };
    }
    {
      name = "path___path_0.12.7.tgz";
      path = fetchurl {
        name = "path___path_0.12.7.tgz";
        url  = "https://registry.yarnpkg.com/path/-/path-0.12.7.tgz";
        sha1 = "d4dc2a506c4ce2197eb481ebfcd5b36c0140b10f";
      };
    }
    {
      name = "https___registry.npmjs.org_pbkdf2___pbkdf2_3.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_pbkdf2___pbkdf2_3.1.1.tgz";
        url  = "https://registry.npmjs.org/pbkdf2/-/pbkdf2-3.1.1.tgz";
        sha1 = "cb8724b0fada984596856d1a6ebafd3584654b94";
      };
    }
    {
      name = "https___registry.npmjs.org_performance_now___performance_now_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_performance_now___performance_now_2.1.0.tgz";
        url  = "https://registry.npmjs.org/performance-now/-/performance-now-2.1.0.tgz";
        sha1 = "6309f4e0e5fa913ec1c69307ae364b4b377c9e7b";
      };
    }
    {
      name = "https___registry.npmjs.org_physical_cpu_count___physical_cpu_count_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_physical_cpu_count___physical_cpu_count_2.0.0.tgz";
        url  = "https://registry.npmjs.org/physical-cpu-count/-/physical-cpu-count-2.0.0.tgz";
        sha1 = "18de2f97e4bf7a9551ad7511942b5496f7aba660";
      };
    }
    {
      name = "https___registry.npmjs.org_picomatch___picomatch_2.2.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_picomatch___picomatch_2.2.2.tgz";
        url  = "https://registry.npmjs.org/picomatch/-/picomatch-2.2.2.tgz";
        sha1 = "21f333e9b6b8eaff02468f5146ea406d345f4dad";
      };
    }
    {
      name = "https___registry.npmjs.org_pirates___pirates_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_pirates___pirates_4.0.1.tgz";
        url  = "https://registry.npmjs.org/pirates/-/pirates-4.0.1.tgz";
        sha1 = "643a92caf894566f91b2b986d2c66950a8e2fb87";
      };
    }
    {
      name = "https___registry.npmjs.org_pkg_dir___pkg_dir_4.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_pkg_dir___pkg_dir_4.2.0.tgz";
        url  = "https://registry.npmjs.org/pkg-dir/-/pkg-dir-4.2.0.tgz";
        sha1 = "f099133df7ede422e81d1d8448270eeb3e4261f3";
      };
    }
    {
      name = "https___registry.npmjs.org_pn___pn_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_pn___pn_1.1.0.tgz";
        url  = "https://registry.npmjs.org/pn/-/pn-1.1.0.tgz";
        sha1 = "e2f4cef0e219f463c179ab37463e4e1ecdccbafb";
      };
    }
    {
      name = "https___registry.npmjs.org_posix_character_classes___posix_character_classes_0.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_posix_character_classes___posix_character_classes_0.1.1.tgz";
        url  = "https://registry.npmjs.org/posix-character-classes/-/posix-character-classes-0.1.1.tgz";
        sha1 = "01eac0fe3b5af71a2a6c02feabb8c1fef7e00eab";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_calc___postcss_calc_7.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_calc___postcss_calc_7.0.5.tgz";
        url  = "https://registry.npmjs.org/postcss-calc/-/postcss-calc-7.0.5.tgz";
        sha1 = "f8a6e99f12e619c2ebc23cf6c486fdc15860933e";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_colormin___postcss_colormin_4.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_colormin___postcss_colormin_4.0.3.tgz";
        url  = "https://registry.npmjs.org/postcss-colormin/-/postcss-colormin-4.0.3.tgz";
        sha1 = "ae060bce93ed794ac71264f08132d550956bd381";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_convert_values___postcss_convert_values_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_convert_values___postcss_convert_values_4.0.1.tgz";
        url  = "https://registry.npmjs.org/postcss-convert-values/-/postcss-convert-values-4.0.1.tgz";
        sha1 = "ca3813ed4da0f812f9d43703584e449ebe189a7f";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_discard_comments___postcss_discard_comments_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_discard_comments___postcss_discard_comments_4.0.2.tgz";
        url  = "https://registry.npmjs.org/postcss-discard-comments/-/postcss-discard-comments-4.0.2.tgz";
        sha1 = "1fbabd2c246bff6aaad7997b2b0918f4d7af4033";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_discard_duplicates___postcss_discard_duplicates_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_discard_duplicates___postcss_discard_duplicates_4.0.2.tgz";
        url  = "https://registry.npmjs.org/postcss-discard-duplicates/-/postcss-discard-duplicates-4.0.2.tgz";
        sha1 = "3fe133cd3c82282e550fc9b239176a9207b784eb";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_discard_empty___postcss_discard_empty_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_discard_empty___postcss_discard_empty_4.0.1.tgz";
        url  = "https://registry.npmjs.org/postcss-discard-empty/-/postcss-discard-empty-4.0.1.tgz";
        sha1 = "c8c951e9f73ed9428019458444a02ad90bb9f765";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_discard_overridden___postcss_discard_overridden_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_discard_overridden___postcss_discard_overridden_4.0.1.tgz";
        url  = "https://registry.npmjs.org/postcss-discard-overridden/-/postcss-discard-overridden-4.0.1.tgz";
        sha1 = "652aef8a96726f029f5e3e00146ee7a4e755ff57";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_merge_longhand___postcss_merge_longhand_4.0.11.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_merge_longhand___postcss_merge_longhand_4.0.11.tgz";
        url  = "https://registry.npmjs.org/postcss-merge-longhand/-/postcss-merge-longhand-4.0.11.tgz";
        sha1 = "62f49a13e4a0ee04e7b98f42bb16062ca2549e24";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_merge_rules___postcss_merge_rules_4.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_merge_rules___postcss_merge_rules_4.0.3.tgz";
        url  = "https://registry.npmjs.org/postcss-merge-rules/-/postcss-merge-rules-4.0.3.tgz";
        sha1 = "362bea4ff5a1f98e4075a713c6cb25aefef9a650";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_minify_font_values___postcss_minify_font_values_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_minify_font_values___postcss_minify_font_values_4.0.2.tgz";
        url  = "https://registry.npmjs.org/postcss-minify-font-values/-/postcss-minify-font-values-4.0.2.tgz";
        sha1 = "cd4c344cce474343fac5d82206ab2cbcb8afd5a6";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_minify_gradients___postcss_minify_gradients_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_minify_gradients___postcss_minify_gradients_4.0.2.tgz";
        url  = "https://registry.npmjs.org/postcss-minify-gradients/-/postcss-minify-gradients-4.0.2.tgz";
        sha1 = "93b29c2ff5099c535eecda56c4aa6e665a663471";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_minify_params___postcss_minify_params_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_minify_params___postcss_minify_params_4.0.2.tgz";
        url  = "https://registry.npmjs.org/postcss-minify-params/-/postcss-minify-params-4.0.2.tgz";
        sha1 = "6b9cef030c11e35261f95f618c90036d680db874";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_minify_selectors___postcss_minify_selectors_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_minify_selectors___postcss_minify_selectors_4.0.2.tgz";
        url  = "https://registry.npmjs.org/postcss-minify-selectors/-/postcss-minify-selectors-4.0.2.tgz";
        sha1 = "e2e5eb40bfee500d0cd9243500f5f8ea4262fbd8";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_modules_extract_imports___postcss_modules_extract_imports_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_modules_extract_imports___postcss_modules_extract_imports_1.1.0.tgz";
        url  = "https://registry.npmjs.org/postcss-modules-extract-imports/-/postcss-modules-extract-imports-1.1.0.tgz";
        sha1 = "b614c9720be6816eaee35fb3a5faa1dba6a05ddb";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_modules_local_by_default___postcss_modules_local_by_default_1.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_modules_local_by_default___postcss_modules_local_by_default_1.2.0.tgz";
        url  = "https://registry.npmjs.org/postcss-modules-local-by-default/-/postcss-modules-local-by-default-1.2.0.tgz";
        sha1 = "f7d80c398c5a393fa7964466bd19500a7d61c069";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_modules_scope___postcss_modules_scope_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_modules_scope___postcss_modules_scope_1.1.0.tgz";
        url  = "https://registry.npmjs.org/postcss-modules-scope/-/postcss-modules-scope-1.1.0.tgz";
        sha1 = "d6ea64994c79f97b62a72b426fbe6056a194bb90";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_modules_values___postcss_modules_values_1.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_modules_values___postcss_modules_values_1.3.0.tgz";
        url  = "https://registry.npmjs.org/postcss-modules-values/-/postcss-modules-values-1.3.0.tgz";
        sha1 = "ecffa9d7e192518389f42ad0e83f72aec456ea20";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_normalize_charset___postcss_normalize_charset_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_normalize_charset___postcss_normalize_charset_4.0.1.tgz";
        url  = "https://registry.npmjs.org/postcss-normalize-charset/-/postcss-normalize-charset-4.0.1.tgz";
        sha1 = "8b35add3aee83a136b0471e0d59be58a50285dd4";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_normalize_display_values___postcss_normalize_display_values_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_normalize_display_values___postcss_normalize_display_values_4.0.2.tgz";
        url  = "https://registry.npmjs.org/postcss-normalize-display-values/-/postcss-normalize-display-values-4.0.2.tgz";
        sha1 = "0dbe04a4ce9063d4667ed2be476bb830c825935a";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_normalize_positions___postcss_normalize_positions_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_normalize_positions___postcss_normalize_positions_4.0.2.tgz";
        url  = "https://registry.npmjs.org/postcss-normalize-positions/-/postcss-normalize-positions-4.0.2.tgz";
        sha1 = "05f757f84f260437378368a91f8932d4b102917f";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_normalize_repeat_style___postcss_normalize_repeat_style_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_normalize_repeat_style___postcss_normalize_repeat_style_4.0.2.tgz";
        url  = "https://registry.npmjs.org/postcss-normalize-repeat-style/-/postcss-normalize-repeat-style-4.0.2.tgz";
        sha1 = "c4ebbc289f3991a028d44751cbdd11918b17910c";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_normalize_string___postcss_normalize_string_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_normalize_string___postcss_normalize_string_4.0.2.tgz";
        url  = "https://registry.npmjs.org/postcss-normalize-string/-/postcss-normalize-string-4.0.2.tgz";
        sha1 = "cd44c40ab07a0c7a36dc5e99aace1eca4ec2690c";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_normalize_timing_functions___postcss_normalize_timing_functions_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_normalize_timing_functions___postcss_normalize_timing_functions_4.0.2.tgz";
        url  = "https://registry.npmjs.org/postcss-normalize-timing-functions/-/postcss-normalize-timing-functions-4.0.2.tgz";
        sha1 = "8e009ca2a3949cdaf8ad23e6b6ab99cb5e7d28d9";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_normalize_unicode___postcss_normalize_unicode_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_normalize_unicode___postcss_normalize_unicode_4.0.1.tgz";
        url  = "https://registry.npmjs.org/postcss-normalize-unicode/-/postcss-normalize-unicode-4.0.1.tgz";
        sha1 = "841bd48fdcf3019ad4baa7493a3d363b52ae1cfb";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_normalize_url___postcss_normalize_url_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_normalize_url___postcss_normalize_url_4.0.1.tgz";
        url  = "https://registry.npmjs.org/postcss-normalize-url/-/postcss-normalize-url-4.0.1.tgz";
        sha1 = "10e437f86bc7c7e58f7b9652ed878daaa95faae1";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_normalize_whitespace___postcss_normalize_whitespace_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_normalize_whitespace___postcss_normalize_whitespace_4.0.2.tgz";
        url  = "https://registry.npmjs.org/postcss-normalize-whitespace/-/postcss-normalize-whitespace-4.0.2.tgz";
        sha1 = "bf1d4070fe4fcea87d1348e825d8cc0c5faa7d82";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_ordered_values___postcss_ordered_values_4.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_ordered_values___postcss_ordered_values_4.1.2.tgz";
        url  = "https://registry.npmjs.org/postcss-ordered-values/-/postcss-ordered-values-4.1.2.tgz";
        sha1 = "0cf75c820ec7d5c4d280189559e0b571ebac0eee";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_reduce_initial___postcss_reduce_initial_4.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_reduce_initial___postcss_reduce_initial_4.0.3.tgz";
        url  = "https://registry.npmjs.org/postcss-reduce-initial/-/postcss-reduce-initial-4.0.3.tgz";
        sha1 = "7fd42ebea5e9c814609639e2c2e84ae270ba48df";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_reduce_transforms___postcss_reduce_transforms_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_reduce_transforms___postcss_reduce_transforms_4.0.2.tgz";
        url  = "https://registry.npmjs.org/postcss-reduce-transforms/-/postcss-reduce-transforms-4.0.2.tgz";
        sha1 = "17efa405eacc6e07be3414a5ca2d1074681d4e29";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_selector_parser___postcss_selector_parser_6.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_selector_parser___postcss_selector_parser_6.0.2.tgz";
        url  = "https://registry.npmjs.org/postcss-selector-parser/-/postcss-selector-parser-6.0.2.tgz";
        sha1 = "934cf799d016c83411859e09dcecade01286ec5c";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_selector_parser___postcss_selector_parser_3.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_selector_parser___postcss_selector_parser_3.1.2.tgz";
        url  = "https://registry.npmjs.org/postcss-selector-parser/-/postcss-selector-parser-3.1.2.tgz";
        sha1 = "b310f5c4c0fdaf76f94902bbaa30db6aa84f5270";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_selector_parser___postcss_selector_parser_6.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_selector_parser___postcss_selector_parser_6.0.4.tgz";
        url  = "https://registry.npmjs.org/postcss-selector-parser/-/postcss-selector-parser-6.0.4.tgz";
        sha1 = "56075a1380a04604c38b063ea7767a129af5c2b3";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_svgo___postcss_svgo_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_svgo___postcss_svgo_4.0.2.tgz";
        url  = "https://registry.npmjs.org/postcss-svgo/-/postcss-svgo-4.0.2.tgz";
        sha1 = "17b997bc711b333bab143aaed3b8d3d6e3d38258";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_unique_selectors___postcss_unique_selectors_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_unique_selectors___postcss_unique_selectors_4.0.1.tgz";
        url  = "https://registry.npmjs.org/postcss-unique-selectors/-/postcss-unique-selectors-4.0.1.tgz";
        sha1 = "9446911f3289bfd64c6d680f073c03b1f9ee4bac";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_value_parser___postcss_value_parser_3.3.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_value_parser___postcss_value_parser_3.3.1.tgz";
        url  = "https://registry.npmjs.org/postcss-value-parser/-/postcss-value-parser-3.3.1.tgz";
        sha1 = "9ff822547e2893213cf1c30efa51ac5fd1ba8281";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_value_parser___postcss_value_parser_4.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_value_parser___postcss_value_parser_4.1.0.tgz";
        url  = "https://registry.npmjs.org/postcss-value-parser/-/postcss-value-parser-4.1.0.tgz";
        sha1 = "443f6a20ced6481a2bda4fa8532a6e55d789a2cb";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss___postcss_6.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss___postcss_6.0.1.tgz";
        url  = "https://registry.npmjs.org/postcss/-/postcss-6.0.1.tgz";
        sha1 = "000dbd1f8eef217aa368b9a212c5fc40b2a8f3f2";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss___postcss_7.0.32.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss___postcss_7.0.32.tgz";
        url  = "https://registry.npmjs.org/postcss/-/postcss-7.0.32.tgz";
        sha1 = "4310d6ee347053da3433db2be492883d62cec59d";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss___postcss_6.0.23.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss___postcss_6.0.23.tgz";
        url  = "https://registry.npmjs.org/postcss/-/postcss-6.0.23.tgz";
        sha1 = "61c82cc328ac60e677645f979054eb98bc0e3324";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss___postcss_7.0.35.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss___postcss_7.0.35.tgz";
        url  = "https://registry.npmjs.org/postcss/-/postcss-7.0.35.tgz";
        sha1 = "d2be00b998f7f211d8a276974079f2e92b970e24";
      };
    }
    {
      name = "https___registry.npmjs.org_posthtml_parser___posthtml_parser_0.4.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_posthtml_parser___posthtml_parser_0.4.2.tgz";
        url  = "https://registry.npmjs.org/posthtml-parser/-/posthtml-parser-0.4.2.tgz";
        sha1 = "a132bbdf0cd4bc199d34f322f5c1599385d7c6c1";
      };
    }
    {
      name = "https___registry.npmjs.org_posthtml_parser___posthtml_parser_0.5.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_posthtml_parser___posthtml_parser_0.5.3.tgz";
        url  = "https://registry.npmjs.org/posthtml-parser/-/posthtml-parser-0.5.3.tgz";
        sha1 = "e95b92e57d98da50b443e116fcee39466cd9012e";
      };
    }
    {
      name = "https___registry.npmjs.org_posthtml_render___posthtml_render_1.4.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_posthtml_render___posthtml_render_1.4.0.tgz";
        url  = "https://registry.npmjs.org/posthtml-render/-/posthtml-render-1.4.0.tgz";
        sha1 = "40114070c45881cacb93347dae3eff53afbcff13";
      };
    }
    {
      name = "https___registry.npmjs.org_posthtml___posthtml_0.11.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_posthtml___posthtml_0.11.6.tgz";
        url  = "https://registry.npmjs.org/posthtml/-/posthtml-0.11.6.tgz";
        sha1 = "e349d51af7929d0683b9d8c3abd8166beecc90a8";
      };
    }
    {
      name = "https___registry.npmjs.org_posthtml___posthtml_0.13.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_posthtml___posthtml_0.13.4.tgz";
        url  = "https://registry.npmjs.org/posthtml/-/posthtml-0.13.4.tgz";
        sha1 = "ad81b3fa62b85f81ccdb5710f4ec375a4ed94934";
      };
    }
    {
      name = "https___registry.npmjs.org_preact___preact_10.5.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_preact___preact_10.5.7.tgz";
        url  = "https://registry.npmjs.org/preact/-/preact-10.5.7.tgz";
        sha1 = "f1d84725539e18f7ccbea937cf3db5895661dbd3";
      };
    }
    {
      name = "https___registry.npmjs.org_prelude_ls___prelude_ls_1.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_prelude_ls___prelude_ls_1.1.2.tgz";
        url  = "https://registry.npmjs.org/prelude-ls/-/prelude-ls-1.1.2.tgz";
        sha1 = "21932a549f5e52ffd9a827f570e04be62a97da54";
      };
    }
    {
      name = "https___registry.npmjs.org_pretty_format___pretty_format_25.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_pretty_format___pretty_format_25.5.0.tgz";
        url  = "https://registry.npmjs.org/pretty-format/-/pretty-format-25.5.0.tgz";
        sha1 = "7873c1d774f682c34b8d48b6743a2bf2ac55791a";
      };
    }
    {
      name = "https___registry.npmjs.org_process_nextick_args___process_nextick_args_2.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_process_nextick_args___process_nextick_args_2.0.1.tgz";
        url  = "https://registry.npmjs.org/process-nextick-args/-/process-nextick-args-2.0.1.tgz";
        sha1 = "7820d9b16120cc55ca9ae7792680ae7dba6d7fe2";
      };
    }
    {
      name = "https___registry.npmjs.org_process___process_0.11.10.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_process___process_0.11.10.tgz";
        url  = "https://registry.npmjs.org/process/-/process-0.11.10.tgz";
        sha1 = "7332300e840161bda3e69a1d1d91a7d4bc16f182";
      };
    }
    {
      name = "https___registry.npmjs.org_prompts___prompts_2.4.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_prompts___prompts_2.4.0.tgz";
        url  = "https://registry.npmjs.org/prompts/-/prompts-2.4.0.tgz";
        sha1 = "4aa5de0723a231d1ee9121c40fdf663df73f61d7";
      };
    }
    {
      name = "https___registry.npmjs.org_psl___psl_1.8.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_psl___psl_1.8.0.tgz";
        url  = "https://registry.npmjs.org/psl/-/psl-1.8.0.tgz";
        sha1 = "9326f8bcfb013adcc005fdff056acce020e51c24";
      };
    }
    {
      name = "https___registry.npmjs.org_public_encrypt___public_encrypt_4.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_public_encrypt___public_encrypt_4.0.3.tgz";
        url  = "https://registry.npmjs.org/public-encrypt/-/public-encrypt-4.0.3.tgz";
        sha1 = "4fcc9d77a07e48ba7527e7cbe0de33d0701331e0";
      };
    }
    {
      name = "https___registry.npmjs.org_pump___pump_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_pump___pump_3.0.0.tgz";
        url  = "https://registry.npmjs.org/pump/-/pump-3.0.0.tgz";
        sha1 = "b4a2116815bde2f4e1ea602354e8c75565107a64";
      };
    }
    {
      name = "https___registry.npmjs.org_punycode___punycode_1.3.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_punycode___punycode_1.3.2.tgz";
        url  = "https://registry.npmjs.org/punycode/-/punycode-1.3.2.tgz";
        sha1 = "9653a036fb7c1ee42342f2325cceefea3926c48d";
      };
    }
    {
      name = "https___registry.npmjs.org_punycode___punycode_1.4.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_punycode___punycode_1.4.1.tgz";
        url  = "https://registry.npmjs.org/punycode/-/punycode-1.4.1.tgz";
        sha1 = "c0d5a63b2718800ad8e1eb0fa5269c84dd41845e";
      };
    }
    {
      name = "https___registry.npmjs.org_punycode___punycode_2.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_punycode___punycode_2.1.1.tgz";
        url  = "https://registry.npmjs.org/punycode/-/punycode-2.1.1.tgz";
        sha1 = "b58b010ac40c22c5657616c8d2c2c02c7bf479ec";
      };
    }
    {
      name = "https___registry.npmjs.org_purgecss___purgecss_2.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_purgecss___purgecss_2.3.0.tgz";
        url  = "https://registry.npmjs.org/purgecss/-/purgecss-2.3.0.tgz";
        sha1 = "5327587abf5795e6541517af8b190a6fb5488bb3";
      };
    }
    {
      name = "https___registry.npmjs.org_q___q_1.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_q___q_1.5.1.tgz";
        url  = "https://registry.npmjs.org/q/-/q-1.5.1.tgz";
        sha1 = "7e32f75b41381291d04611f1bf14109ac00651d7";
      };
    }
    {
      name = "https___registry.npmjs.org_qs___qs_6.5.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_qs___qs_6.5.2.tgz";
        url  = "https://registry.npmjs.org/qs/-/qs-6.5.2.tgz";
        sha1 = "cb3ae806e8740444584ef154ce8ee98d403f3e36";
      };
    }
    {
      name = "https___registry.npmjs.org_querystring_es3___querystring_es3_0.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_querystring_es3___querystring_es3_0.2.1.tgz";
        url  = "https://registry.npmjs.org/querystring-es3/-/querystring-es3-0.2.1.tgz";
        sha1 = "9ec61f79049875707d69414596fd907a4d711e73";
      };
    }
    {
      name = "https___registry.npmjs.org_querystring___querystring_0.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_querystring___querystring_0.2.0.tgz";
        url  = "https://registry.npmjs.org/querystring/-/querystring-0.2.0.tgz";
        sha1 = "b209849203bb25df820da756e747005878521620";
      };
    }
    {
      name = "https___registry.npmjs.org_quote_stream___quote_stream_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_quote_stream___quote_stream_1.0.2.tgz";
        url  = "https://registry.npmjs.org/quote-stream/-/quote-stream-1.0.2.tgz";
        sha1 = "84963f8c9c26b942e153feeb53aae74652b7e0b2";
      };
    }
    {
      name = "https___registry.npmjs.org_randombytes___randombytes_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_randombytes___randombytes_2.1.0.tgz";
        url  = "https://registry.npmjs.org/randombytes/-/randombytes-2.1.0.tgz";
        sha1 = "df6f84372f0270dc65cdf6291349ab7a473d4f2a";
      };
    }
    {
      name = "https___registry.npmjs.org_randomfill___randomfill_1.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_randomfill___randomfill_1.0.4.tgz";
        url  = "https://registry.npmjs.org/randomfill/-/randomfill-1.0.4.tgz";
        sha1 = "c92196fc86ab42be983f1bf31778224931d61458";
      };
    }
    {
      name = "https___registry.npmjs.org_range_parser___range_parser_1.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_range_parser___range_parser_1.2.1.tgz";
        url  = "https://registry.npmjs.org/range-parser/-/range-parser-1.2.1.tgz";
        sha1 = "3cf37023d199e1c24d1a55b84800c2f3e6468031";
      };
    }
    {
      name = "https___registry.npmjs.org_react_is___react_is_16.13.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_react_is___react_is_16.13.1.tgz";
        url  = "https://registry.npmjs.org/react-is/-/react-is-16.13.1.tgz";
        sha1 = "789729a4dc36de2999dc156dd6c1d9c18cea56a4";
      };
    }
    {
      name = "https___registry.npmjs.org_read_pkg_up___read_pkg_up_7.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_read_pkg_up___read_pkg_up_7.0.1.tgz";
        url  = "https://registry.npmjs.org/read-pkg-up/-/read-pkg-up-7.0.1.tgz";
        sha1 = "f3a6135758459733ae2b95638056e1854e7ef507";
      };
    }
    {
      name = "https___registry.npmjs.org_read_pkg___read_pkg_5.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_read_pkg___read_pkg_5.2.0.tgz";
        url  = "https://registry.npmjs.org/read-pkg/-/read-pkg-5.2.0.tgz";
        sha1 = "7bf295438ca5a33e56cd30e053b34ee7250c93cc";
      };
    }
    {
      name = "https___registry.npmjs.org_readable_stream___readable_stream_2.3.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_readable_stream___readable_stream_2.3.7.tgz";
        url  = "https://registry.npmjs.org/readable-stream/-/readable-stream-2.3.7.tgz";
        sha1 = "1eca1cf711aef814c04f62252a36a62f6cb23b57";
      };
    }
    {
      name = "https___registry.npmjs.org_readable_stream___readable_stream_3.6.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_readable_stream___readable_stream_3.6.0.tgz";
        url  = "https://registry.npmjs.org/readable-stream/-/readable-stream-3.6.0.tgz";
        sha1 = "337bbda3adc0706bd3e024426a286d4b4b2c9198";
      };
    }
    {
      name = "https___registry.npmjs.org_readdirp___readdirp_2.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_readdirp___readdirp_2.2.1.tgz";
        url  = "https://registry.npmjs.org/readdirp/-/readdirp-2.2.1.tgz";
        sha1 = "0e87622a3325aa33e892285caf8b4e846529a525";
      };
    }
    {
      name = "https___registry.npmjs.org_realpath_native___realpath_native_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_realpath_native___realpath_native_2.0.0.tgz";
        url  = "https://registry.npmjs.org/realpath-native/-/realpath-native-2.0.0.tgz";
        sha1 = "7377ac429b6e1fd599dc38d08ed942d0d7beb866";
      };
    }
    {
      name = "https___registry.npmjs.org_regenerate_unicode_properties___regenerate_unicode_properties_8.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_regenerate_unicode_properties___regenerate_unicode_properties_8.2.0.tgz";
        url  = "https://registry.npmjs.org/regenerate-unicode-properties/-/regenerate-unicode-properties-8.2.0.tgz";
        sha1 = "e5de7111d655e7ba60c057dbe9ff37c87e65cdec";
      };
    }
    {
      name = "https___registry.npmjs.org_regenerate___regenerate_1.4.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_regenerate___regenerate_1.4.2.tgz";
        url  = "https://registry.npmjs.org/regenerate/-/regenerate-1.4.2.tgz";
        sha1 = "b9346d8827e8f5a32f7ba29637d398b69014848a";
      };
    }
    {
      name = "https___registry.npmjs.org_regenerator_runtime___regenerator_runtime_0.11.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_regenerator_runtime___regenerator_runtime_0.11.1.tgz";
        url  = "https://registry.npmjs.org/regenerator-runtime/-/regenerator-runtime-0.11.1.tgz";
        sha1 = "be05ad7f9bf7d22e056f9726cee5017fbf19e2e9";
      };
    }
    {
      name = "https___registry.npmjs.org_regenerator_runtime___regenerator_runtime_0.13.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_regenerator_runtime___regenerator_runtime_0.13.7.tgz";
        url  = "https://registry.npmjs.org/regenerator-runtime/-/regenerator-runtime-0.13.7.tgz";
        sha1 = "cac2dacc8a1ea675feaabaeb8ae833898ae46f55";
      };
    }
    {
      name = "https___registry.npmjs.org_regenerator_transform___regenerator_transform_0.14.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_regenerator_transform___regenerator_transform_0.14.5.tgz";
        url  = "https://registry.npmjs.org/regenerator-transform/-/regenerator-transform-0.14.5.tgz";
        sha1 = "c98da154683671c9c4dcb16ece736517e1b7feb4";
      };
    }
    {
      name = "https___registry.npmjs.org_regex_not___regex_not_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_regex_not___regex_not_1.0.2.tgz";
        url  = "https://registry.npmjs.org/regex-not/-/regex-not-1.0.2.tgz";
        sha1 = "1f4ece27e00b0b65e0247a6810e6a85d83a5752c";
      };
    }
    {
      name = "https___registry.npmjs.org_regexpu_core___regexpu_core_4.7.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_regexpu_core___regexpu_core_4.7.1.tgz";
        url  = "https://registry.npmjs.org/regexpu-core/-/regexpu-core-4.7.1.tgz";
        sha1 = "2dea5a9a07233298fbf0db91fa9abc4c6e0f8ad6";
      };
    }
    {
      name = "https___registry.npmjs.org_regjsgen___regjsgen_0.5.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_regjsgen___regjsgen_0.5.2.tgz";
        url  = "https://registry.npmjs.org/regjsgen/-/regjsgen-0.5.2.tgz";
        sha1 = "92ff295fb1deecbf6ecdab2543d207e91aa33733";
      };
    }
    {
      name = "https___registry.npmjs.org_regjsparser___regjsparser_0.6.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_regjsparser___regjsparser_0.6.4.tgz";
        url  = "https://registry.npmjs.org/regjsparser/-/regjsparser-0.6.4.tgz";
        sha1 = "a769f8684308401a66e9b529d2436ff4d0666272";
      };
    }
    {
      name = "https___registry.npmjs.org_relateurl___relateurl_0.2.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_relateurl___relateurl_0.2.7.tgz";
        url  = "https://registry.npmjs.org/relateurl/-/relateurl-0.2.7.tgz";
        sha1 = "54dbf377e51440aca90a4cd274600d3ff2d888a9";
      };
    }
    {
      name = "https___registry.npmjs.org_remove_trailing_separator___remove_trailing_separator_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_remove_trailing_separator___remove_trailing_separator_1.1.0.tgz";
        url  = "https://registry.npmjs.org/remove-trailing-separator/-/remove-trailing-separator-1.1.0.tgz";
        sha1 = "c24bce2a283adad5bc3f58e0d48249b92379d8ef";
      };
    }
    {
      name = "https___registry.npmjs.org_repeat_element___repeat_element_1.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_repeat_element___repeat_element_1.1.3.tgz";
        url  = "https://registry.npmjs.org/repeat-element/-/repeat-element-1.1.3.tgz";
        sha1 = "782e0d825c0c5a3bb39731f84efee6b742e6b1ce";
      };
    }
    {
      name = "https___registry.npmjs.org_repeat_string___repeat_string_1.6.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_repeat_string___repeat_string_1.6.1.tgz";
        url  = "https://registry.npmjs.org/repeat-string/-/repeat-string-1.6.1.tgz";
        sha1 = "8dcae470e1c88abc2d600fff4a776286da75e637";
      };
    }
    {
      name = "https___registry.npmjs.org_request_promise_core___request_promise_core_1.1.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_request_promise_core___request_promise_core_1.1.4.tgz";
        url  = "https://registry.npmjs.org/request-promise-core/-/request-promise-core-1.1.4.tgz";
        sha1 = "3eedd4223208d419867b78ce815167d10593a22f";
      };
    }
    {
      name = "https___registry.npmjs.org_request_promise_native___request_promise_native_1.0.9.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_request_promise_native___request_promise_native_1.0.9.tgz";
        url  = "https://registry.npmjs.org/request-promise-native/-/request-promise-native-1.0.9.tgz";
        sha1 = "e407120526a5efdc9a39b28a5679bf47b9d9dc28";
      };
    }
    {
      name = "https___registry.npmjs.org_request___request_2.88.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_request___request_2.88.2.tgz";
        url  = "https://registry.npmjs.org/request/-/request-2.88.2.tgz";
        sha1 = "d73c918731cb5a87da047e207234146f664d12b3";
      };
    }
    {
      name = "https___registry.npmjs.org_require_directory___require_directory_2.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_require_directory___require_directory_2.1.1.tgz";
        url  = "https://registry.npmjs.org/require-directory/-/require-directory-2.1.1.tgz";
        sha1 = "8c64ad5fd30dab1c976e2344ffe7f792a6a6df42";
      };
    }
    {
      name = "https___registry.npmjs.org_require_main_filename___require_main_filename_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_require_main_filename___require_main_filename_2.0.0.tgz";
        url  = "https://registry.npmjs.org/require-main-filename/-/require-main-filename-2.0.0.tgz";
        sha1 = "d0b329ecc7cc0f61649f62215be69af54aa8989b";
      };
    }
    {
      name = "https___registry.npmjs.org_resolve_cwd___resolve_cwd_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_resolve_cwd___resolve_cwd_3.0.0.tgz";
        url  = "https://registry.npmjs.org/resolve-cwd/-/resolve-cwd-3.0.0.tgz";
        sha1 = "0f0075f1bb2544766cf73ba6a6e2adfebcb13f2d";
      };
    }
    {
      name = "https___registry.npmjs.org_resolve_from___resolve_from_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_resolve_from___resolve_from_3.0.0.tgz";
        url  = "https://registry.npmjs.org/resolve-from/-/resolve-from-3.0.0.tgz";
        sha1 = "b22c7af7d9d6881bc8b6e653335eebcb0a188748";
      };
    }
    {
      name = "https___registry.npmjs.org_resolve_from___resolve_from_5.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_resolve_from___resolve_from_5.0.0.tgz";
        url  = "https://registry.npmjs.org/resolve-from/-/resolve-from-5.0.0.tgz";
        sha1 = "c35225843df8f776df21c57557bc087e9dfdfc69";
      };
    }
    {
      name = "https___registry.npmjs.org_resolve_url___resolve_url_0.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_resolve_url___resolve_url_0.2.1.tgz";
        url  = "https://registry.npmjs.org/resolve-url/-/resolve-url-0.2.1.tgz";
        sha1 = "2c637fe77c893afd2a663fe21aa9080068e2052a";
      };
    }
    {
      name = "https___registry.npmjs.org_resolve___resolve_1.1.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_resolve___resolve_1.1.7.tgz";
        url  = "https://registry.npmjs.org/resolve/-/resolve-1.1.7.tgz";
        sha1 = "203114d82ad2c5ed9e8e0411b3932875e889e97b";
      };
    }
    {
      name = "https___registry.npmjs.org_resolve___resolve_1.19.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_resolve___resolve_1.19.0.tgz";
        url  = "https://registry.npmjs.org/resolve/-/resolve-1.19.0.tgz";
        sha1 = "1af5bf630409734a067cae29318aac7fa29a267c";
      };
    }
    {
      name = "https___registry.npmjs.org_restore_cursor___restore_cursor_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_restore_cursor___restore_cursor_2.0.0.tgz";
        url  = "https://registry.npmjs.org/restore-cursor/-/restore-cursor-2.0.0.tgz";
        sha1 = "9f7ee287f82fd326d4fd162923d62129eee0dfaf";
      };
    }
    {
      name = "https___registry.npmjs.org_ret___ret_0.1.15.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ret___ret_0.1.15.tgz";
        url  = "https://registry.npmjs.org/ret/-/ret-0.1.15.tgz";
        sha1 = "b8a4825d5bdb1fc3f6f53c2bc33f81388681c7bc";
      };
    }
    {
      name = "https___registry.npmjs.org_rgb_regex___rgb_regex_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_rgb_regex___rgb_regex_1.0.1.tgz";
        url  = "https://registry.npmjs.org/rgb-regex/-/rgb-regex-1.0.1.tgz";
        sha1 = "c0e0d6882df0e23be254a475e8edd41915feaeb1";
      };
    }
    {
      name = "https___registry.npmjs.org_rgba_regex___rgba_regex_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_rgba_regex___rgba_regex_1.0.0.tgz";
        url  = "https://registry.npmjs.org/rgba-regex/-/rgba-regex-1.0.0.tgz";
        sha1 = "43374e2e2ca0968b0ef1523460b7d730ff22eeb3";
      };
    }
    {
      name = "https___registry.npmjs.org_rimraf___rimraf_2.7.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_rimraf___rimraf_2.7.1.tgz";
        url  = "https://registry.npmjs.org/rimraf/-/rimraf-2.7.1.tgz";
        sha1 = "35797f13a7fdadc566142c29d4f07ccad483e3ec";
      };
    }
    {
      name = "https___registry.npmjs.org_rimraf___rimraf_3.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_rimraf___rimraf_3.0.2.tgz";
        url  = "https://registry.npmjs.org/rimraf/-/rimraf-3.0.2.tgz";
        sha1 = "f1a5402ba6220ad52cc1282bac1ae3aa49fd061a";
      };
    }
    {
      name = "https___registry.npmjs.org_ripemd160___ripemd160_2.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ripemd160___ripemd160_2.0.2.tgz";
        url  = "https://registry.npmjs.org/ripemd160/-/ripemd160-2.0.2.tgz";
        sha1 = "a1c1a6f624751577ba5d07914cbc92850585890c";
      };
    }
    {
      name = "https___registry.npmjs.org_rsvp___rsvp_4.8.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_rsvp___rsvp_4.8.5.tgz";
        url  = "https://registry.npmjs.org/rsvp/-/rsvp-4.8.5.tgz";
        sha1 = "c8f155311d167f68f21e168df71ec5b083113734";
      };
    }
    {
      name = "https___registry.npmjs.org_safe_buffer___safe_buffer_5.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_safe_buffer___safe_buffer_5.2.1.tgz";
        url  = "https://registry.npmjs.org/safe-buffer/-/safe-buffer-5.2.1.tgz";
        sha1 = "1eaf9fa9bdb1fdd4ec75f58f9cdb4e6b7827eec6";
      };
    }
    {
      name = "https___registry.npmjs.org_safe_buffer___safe_buffer_5.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_safe_buffer___safe_buffer_5.1.2.tgz";
        url  = "https://registry.npmjs.org/safe-buffer/-/safe-buffer-5.1.2.tgz";
        sha1 = "991ec69d296e0313747d59bdfd2b745c35f8828d";
      };
    }
    {
      name = "https___registry.npmjs.org_safe_regex___safe_regex_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_safe_regex___safe_regex_1.1.0.tgz";
        url  = "https://registry.npmjs.org/safe-regex/-/safe-regex-1.1.0.tgz";
        sha1 = "40a3669f3b077d1e943d44629e157dd48023bf2e";
      };
    }
    {
      name = "https___registry.npmjs.org_safer_buffer___safer_buffer_2.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_safer_buffer___safer_buffer_2.1.2.tgz";
        url  = "https://registry.npmjs.org/safer-buffer/-/safer-buffer-2.1.2.tgz";
        sha1 = "44fa161b0187b9549dd84bb91802f9bd8385cd6a";
      };
    }
    {
      name = "https___registry.npmjs.org_sane___sane_4.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_sane___sane_4.1.0.tgz";
        url  = "https://registry.npmjs.org/sane/-/sane-4.1.0.tgz";
        sha1 = "ed881fd922733a6c461bc189dc2b6c006f3ffded";
      };
    }
    {
      name = "https___registry.npmjs.org_sax___sax_1.2.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_sax___sax_1.2.4.tgz";
        url  = "https://registry.npmjs.org/sax/-/sax-1.2.4.tgz";
        sha1 = "2816234e2378bddc4e5354fab5caa895df7100d9";
      };
    }
    {
      name = "https___registry.npmjs.org_saxes___saxes_3.1.11.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_saxes___saxes_3.1.11.tgz";
        url  = "https://registry.npmjs.org/saxes/-/saxes-3.1.11.tgz";
        sha1 = "d59d1fd332ec92ad98a2e0b2ee644702384b1c5b";
      };
    }
    {
      name = "https___registry.npmjs.org_semver___semver_5.7.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_semver___semver_5.7.1.tgz";
        url  = "https://registry.npmjs.org/semver/-/semver-5.7.1.tgz";
        sha1 = "a954f931aeba508d307bbf069eff0c01c96116f7";
      };
    }
    {
      name = "https___registry.npmjs.org_semver___semver_6.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_semver___semver_6.3.0.tgz";
        url  = "https://registry.npmjs.org/semver/-/semver-6.3.0.tgz";
        sha1 = "ee0a64c8af5e8ceea67687b133761e1becbd1d3d";
      };
    }
    {
      name = "https___registry.npmjs.org_semver___semver_7.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_semver___semver_7.0.0.tgz";
        url  = "https://registry.npmjs.org/semver/-/semver-7.0.0.tgz";
        sha1 = "5f3ca35761e47e05b206c6daff2cf814f0316b8e";
      };
    }
    {
      name = "https___registry.npmjs.org_send___send_0.17.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_send___send_0.17.1.tgz";
        url  = "https://registry.npmjs.org/send/-/send-0.17.1.tgz";
        sha1 = "c1d8b059f7900f7466dd4938bdc44e11ddb376c8";
      };
    }
    {
      name = "https___registry.npmjs.org_serialize_to_js___serialize_to_js_3.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_serialize_to_js___serialize_to_js_3.1.1.tgz";
        url  = "https://registry.npmjs.org/serialize-to-js/-/serialize-to-js-3.1.1.tgz";
        sha1 = "b3e77d0568ee4a60bfe66287f991e104d3a1a4ac";
      };
    }
    {
      name = "https___registry.npmjs.org_serve_static___serve_static_1.14.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_serve_static___serve_static_1.14.1.tgz";
        url  = "https://registry.npmjs.org/serve-static/-/serve-static-1.14.1.tgz";
        sha1 = "666e636dc4f010f7ef29970a88a674320898b2f9";
      };
    }
    {
      name = "https___registry.npmjs.org_set_blocking___set_blocking_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_set_blocking___set_blocking_2.0.0.tgz";
        url  = "https://registry.npmjs.org/set-blocking/-/set-blocking-2.0.0.tgz";
        sha1 = "045f9782d011ae9a6803ddd382b24392b3d890f7";
      };
    }
    {
      name = "https___registry.npmjs.org_set_value___set_value_2.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_set_value___set_value_2.0.1.tgz";
        url  = "https://registry.npmjs.org/set-value/-/set-value-2.0.1.tgz";
        sha1 = "a18d40530e6f07de4228c7defe4227af8cad005b";
      };
    }
    {
      name = "https___registry.npmjs.org_setimmediate___setimmediate_1.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_setimmediate___setimmediate_1.0.5.tgz";
        url  = "https://registry.npmjs.org/setimmediate/-/setimmediate-1.0.5.tgz";
        sha1 = "290cbb232e306942d7d7ea9b83732ab7856f8285";
      };
    }
    {
      name = "https___registry.npmjs.org_setprototypeof___setprototypeof_1.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_setprototypeof___setprototypeof_1.1.1.tgz";
        url  = "https://registry.npmjs.org/setprototypeof/-/setprototypeof-1.1.1.tgz";
        sha1 = "7e95acb24aa92f5885e0abef5ba131330d4ae683";
      };
    }
    {
      name = "https___registry.npmjs.org_sha.js___sha.js_2.4.11.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_sha.js___sha.js_2.4.11.tgz";
        url  = "https://registry.npmjs.org/sha.js/-/sha.js-2.4.11.tgz";
        sha1 = "37a5cf0b81ecbc6943de109ba2960d1b26584ae7";
      };
    }
    {
      name = "https___registry.npmjs.org_shallow_copy___shallow_copy_0.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_shallow_copy___shallow_copy_0.0.1.tgz";
        url  = "https://registry.npmjs.org/shallow-copy/-/shallow-copy-0.0.1.tgz";
        sha1 = "415f42702d73d810330292cc5ee86eae1a11a170";
      };
    }
    {
      name = "https___registry.npmjs.org_shebang_command___shebang_command_1.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_shebang_command___shebang_command_1.2.0.tgz";
        url  = "https://registry.npmjs.org/shebang-command/-/shebang-command-1.2.0.tgz";
        sha1 = "44aac65b695b03398968c39f363fee5deafdf1ea";
      };
    }
    {
      name = "https___registry.npmjs.org_shebang_command___shebang_command_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_shebang_command___shebang_command_2.0.0.tgz";
        url  = "https://registry.npmjs.org/shebang-command/-/shebang-command-2.0.0.tgz";
        sha1 = "ccd0af4f8835fbdc265b82461aaf0c36663f34ea";
      };
    }
    {
      name = "https___registry.npmjs.org_shebang_regex___shebang_regex_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_shebang_regex___shebang_regex_1.0.0.tgz";
        url  = "https://registry.npmjs.org/shebang-regex/-/shebang-regex-1.0.0.tgz";
        sha1 = "da42f49740c0b42db2ca9728571cb190c98efea3";
      };
    }
    {
      name = "https___registry.npmjs.org_shebang_regex___shebang_regex_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_shebang_regex___shebang_regex_3.0.0.tgz";
        url  = "https://registry.npmjs.org/shebang-regex/-/shebang-regex-3.0.0.tgz";
        sha1 = "ae16f1644d873ecad843b0307b143362d4c42172";
      };
    }
    {
      name = "https___registry.npmjs.org_shellwords___shellwords_0.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_shellwords___shellwords_0.1.1.tgz";
        url  = "https://registry.npmjs.org/shellwords/-/shellwords-0.1.1.tgz";
        sha1 = "d6b9181c1a48d397324c84871efbcfc73fc0654b";
      };
    }
    {
      name = "https___registry.npmjs.org_signal_exit___signal_exit_3.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_signal_exit___signal_exit_3.0.3.tgz";
        url  = "https://registry.npmjs.org/signal-exit/-/signal-exit-3.0.3.tgz";
        sha1 = "a1410c2edd8f077b08b4e253c8eacfcaf057461c";
      };
    }
    {
      name = "https___registry.npmjs.org_simple_swizzle___simple_swizzle_0.2.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_simple_swizzle___simple_swizzle_0.2.2.tgz";
        url  = "https://registry.npmjs.org/simple-swizzle/-/simple-swizzle-0.2.2.tgz";
        sha1 = "a4da6b635ffcccca33f70d17cb92592de95e557a";
      };
    }
    {
      name = "https___registry.npmjs.org_sisteransi___sisteransi_1.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_sisteransi___sisteransi_1.0.5.tgz";
        url  = "https://registry.npmjs.org/sisteransi/-/sisteransi-1.0.5.tgz";
        sha1 = "134d681297756437cc05ca01370d3a7a571075ed";
      };
    }
    {
      name = "https___registry.npmjs.org_slash___slash_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_slash___slash_3.0.0.tgz";
        url  = "https://registry.npmjs.org/slash/-/slash-3.0.0.tgz";
        sha1 = "6539be870c165adbd5240220dbe361f1bc4d4634";
      };
    }
    {
      name = "https___registry.npmjs.org_snapdragon_node___snapdragon_node_2.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_snapdragon_node___snapdragon_node_2.1.1.tgz";
        url  = "https://registry.npmjs.org/snapdragon-node/-/snapdragon-node-2.1.1.tgz";
        sha1 = "6c175f86ff14bdb0724563e8f3c1b021a286853b";
      };
    }
    {
      name = "https___registry.npmjs.org_snapdragon_util___snapdragon_util_3.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_snapdragon_util___snapdragon_util_3.0.1.tgz";
        url  = "https://registry.npmjs.org/snapdragon-util/-/snapdragon-util-3.0.1.tgz";
        sha1 = "f956479486f2acd79700693f6f7b805e45ab56e2";
      };
    }
    {
      name = "https___registry.npmjs.org_snapdragon___snapdragon_0.8.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_snapdragon___snapdragon_0.8.2.tgz";
        url  = "https://registry.npmjs.org/snapdragon/-/snapdragon-0.8.2.tgz";
        sha1 = "64922e7c565b0e14204ba1aa7d6964278d25182d";
      };
    }
    {
      name = "https___registry.npmjs.org_source_map_resolve___source_map_resolve_0.5.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_source_map_resolve___source_map_resolve_0.5.3.tgz";
        url  = "https://registry.npmjs.org/source-map-resolve/-/source-map-resolve-0.5.3.tgz";
        sha1 = "190866bece7553e1f8f267a2ee82c606b5509a1a";
      };
    }
    {
      name = "https___registry.npmjs.org_source_map_support___source_map_support_0.5.19.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_source_map_support___source_map_support_0.5.19.tgz";
        url  = "https://registry.npmjs.org/source-map-support/-/source-map-support-0.5.19.tgz";
        sha1 = "a98b62f86dcaf4f67399648c085291ab9e8fed61";
      };
    }
    {
      name = "https___registry.npmjs.org_source_map_url___source_map_url_0.4.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_source_map_url___source_map_url_0.4.0.tgz";
        url  = "https://registry.npmjs.org/source-map-url/-/source-map-url-0.4.0.tgz";
        sha1 = "3e935d7ddd73631b97659956d55128e87b5084a3";
      };
    }
    {
      name = "https___registry.npmjs.org_source_map___source_map_0.6.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_source_map___source_map_0.6.1.tgz";
        url  = "https://registry.npmjs.org/source-map/-/source-map-0.6.1.tgz";
        sha1 = "74722af32e9614e9c287a8d0bbde48b5e2f1a263";
      };
    }
    {
      name = "https___registry.npmjs.org_source_map___source_map_0.5.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_source_map___source_map_0.5.7.tgz";
        url  = "https://registry.npmjs.org/source-map/-/source-map-0.5.7.tgz";
        sha1 = "8a039d2d1021d22d1ea14c80d8ea468ba2ef3fcc";
      };
    }
    {
      name = "https___registry.npmjs.org_source_map___source_map_0.7.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_source_map___source_map_0.7.3.tgz";
        url  = "https://registry.npmjs.org/source-map/-/source-map-0.7.3.tgz";
        sha1 = "5302f8169031735226544092e64981f751750383";
      };
    }
    {
      name = "https___registry.npmjs.org_spdx_correct___spdx_correct_3.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_spdx_correct___spdx_correct_3.1.1.tgz";
        url  = "https://registry.npmjs.org/spdx-correct/-/spdx-correct-3.1.1.tgz";
        sha1 = "dece81ac9c1e6713e5f7d1b6f17d468fa53d89a9";
      };
    }
    {
      name = "https___registry.npmjs.org_spdx_exceptions___spdx_exceptions_2.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_spdx_exceptions___spdx_exceptions_2.3.0.tgz";
        url  = "https://registry.npmjs.org/spdx-exceptions/-/spdx-exceptions-2.3.0.tgz";
        sha1 = "3f28ce1a77a00372683eade4a433183527a2163d";
      };
    }
    {
      name = "https___registry.npmjs.org_spdx_expression_parse___spdx_expression_parse_3.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_spdx_expression_parse___spdx_expression_parse_3.0.1.tgz";
        url  = "https://registry.npmjs.org/spdx-expression-parse/-/spdx-expression-parse-3.0.1.tgz";
        sha1 = "cf70f50482eefdc98e3ce0a6833e4a53ceeba679";
      };
    }
    {
      name = "https___registry.npmjs.org_spdx_license_ids___spdx_license_ids_3.0.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_spdx_license_ids___spdx_license_ids_3.0.7.tgz";
        url  = "https://registry.npmjs.org/spdx-license-ids/-/spdx-license-ids-3.0.7.tgz";
        sha1 = "e9c18a410e5ed7e12442a549fbd8afa767038d65";
      };
    }
    {
      name = "https___registry.npmjs.org_split_string___split_string_3.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_split_string___split_string_3.1.0.tgz";
        url  = "https://registry.npmjs.org/split-string/-/split-string-3.1.0.tgz";
        sha1 = "7cb09dda3a86585705c64b39a6466038682e8fe2";
      };
    }
    {
      name = "https___registry.npmjs.org_sprintf_js___sprintf_js_1.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_sprintf_js___sprintf_js_1.0.3.tgz";
        url  = "https://registry.npmjs.org/sprintf-js/-/sprintf-js-1.0.3.tgz";
        sha1 = "04e6926f662895354f3dd015203633b857297e2c";
      };
    }
    {
      name = "https___registry.npmjs.org_srcset___srcset_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_srcset___srcset_3.0.0.tgz";
        url  = "https://registry.npmjs.org/srcset/-/srcset-3.0.0.tgz";
        sha1 = "8afd8b971362dfc129ae9c1a99b3897301ce6441";
      };
    }
    {
      name = "https___registry.npmjs.org_sshpk___sshpk_1.16.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_sshpk___sshpk_1.16.1.tgz";
        url  = "https://registry.npmjs.org/sshpk/-/sshpk-1.16.1.tgz";
        sha1 = "fb661c0bef29b39db40769ee39fa70093d6f6877";
      };
    }
    {
      name = "https___registry.npmjs.org_stable___stable_0.1.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_stable___stable_0.1.8.tgz";
        url  = "https://registry.npmjs.org/stable/-/stable-0.1.8.tgz";
        sha1 = "836eb3c8382fe2936feaf544631017ce7d47a3cf";
      };
    }
    {
      name = "https___registry.npmjs.org_stack_utils___stack_utils_1.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_stack_utils___stack_utils_1.0.4.tgz";
        url  = "https://registry.npmjs.org/stack-utils/-/stack-utils-1.0.4.tgz";
        sha1 = "4b600971dcfc6aed0cbdf2a8268177cc916c87c8";
      };
    }
    {
      name = "https___registry.npmjs.org_static_eval___static_eval_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_static_eval___static_eval_2.1.0.tgz";
        url  = "https://registry.npmjs.org/static-eval/-/static-eval-2.1.0.tgz";
        sha1 = "a16dbe54522d7fa5ef1389129d813fd47b148014";
      };
    }
    {
      name = "https___registry.npmjs.org_static_extend___static_extend_0.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_static_extend___static_extend_0.1.2.tgz";
        url  = "https://registry.npmjs.org/static-extend/-/static-extend-0.1.2.tgz";
        sha1 = "60809c39cbff55337226fd5e0b520f341f1fb5c6";
      };
    }
    {
      name = "https___registry.npmjs.org_static_module___static_module_2.2.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_static_module___static_module_2.2.5.tgz";
        url  = "https://registry.npmjs.org/static-module/-/static-module-2.2.5.tgz";
        sha1 = "bd40abceae33da6b7afb84a0e4329ff8852bfbbf";
      };
    }
    {
      name = "https___registry.npmjs.org_statuses___statuses_1.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_statuses___statuses_1.5.0.tgz";
        url  = "https://registry.npmjs.org/statuses/-/statuses-1.5.0.tgz";
        sha1 = "161c7dac177659fd9811f43771fa99381478628c";
      };
    }
    {
      name = "https___registry.npmjs.org_stealthy_require___stealthy_require_1.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_stealthy_require___stealthy_require_1.1.1.tgz";
        url  = "https://registry.npmjs.org/stealthy-require/-/stealthy-require-1.1.1.tgz";
        sha1 = "35b09875b4ff49f26a777e509b3090a3226bf24b";
      };
    }
    {
      name = "https___registry.npmjs.org_stream_browserify___stream_browserify_2.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_stream_browserify___stream_browserify_2.0.2.tgz";
        url  = "https://registry.npmjs.org/stream-browserify/-/stream-browserify-2.0.2.tgz";
        sha1 = "87521d38a44aa7ee91ce1cd2a47df0cb49dd660b";
      };
    }
    {
      name = "https___registry.npmjs.org_stream_http___stream_http_2.8.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_stream_http___stream_http_2.8.3.tgz";
        url  = "https://registry.npmjs.org/stream-http/-/stream-http-2.8.3.tgz";
        sha1 = "b2d242469288a5a27ec4fe8933acf623de6514fc";
      };
    }
    {
      name = "https___registry.npmjs.org_string_length___string_length_3.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_string_length___string_length_3.1.0.tgz";
        url  = "https://registry.npmjs.org/string-length/-/string-length-3.1.0.tgz";
        sha1 = "107ef8c23456e187a8abd4a61162ff4ac6e25837";
      };
    }
    {
      name = "https___registry.npmjs.org_string_width___string_width_4.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_string_width___string_width_4.2.0.tgz";
        url  = "https://registry.npmjs.org/string-width/-/string-width-4.2.0.tgz";
        sha1 = "952182c46cc7b2c313d1596e623992bd163b72b5";
      };
    }
    {
      name = "https___registry.npmjs.org_string.prototype.trimend___string.prototype.trimend_1.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_string.prototype.trimend___string.prototype.trimend_1.0.3.tgz";
        url  = "https://registry.npmjs.org/string.prototype.trimend/-/string.prototype.trimend-1.0.3.tgz";
        sha1 = "a22bd53cca5c7cf44d7c9d5c732118873d6cd18b";
      };
    }
    {
      name = "https___registry.npmjs.org_string.prototype.trimstart___string.prototype.trimstart_1.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_string.prototype.trimstart___string.prototype.trimstart_1.0.3.tgz";
        url  = "https://registry.npmjs.org/string.prototype.trimstart/-/string.prototype.trimstart-1.0.3.tgz";
        sha1 = "9b4cb590e123bb36564401d59824298de50fd5aa";
      };
    }
    {
      name = "https___registry.npmjs.org_string_decoder___string_decoder_1.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_string_decoder___string_decoder_1.3.0.tgz";
        url  = "https://registry.npmjs.org/string_decoder/-/string_decoder-1.3.0.tgz";
        sha1 = "42f114594a46cf1a8e30b0a84f56c78c3edac21e";
      };
    }
    {
      name = "https___registry.npmjs.org_string_decoder___string_decoder_1.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_string_decoder___string_decoder_1.1.1.tgz";
        url  = "https://registry.npmjs.org/string_decoder/-/string_decoder-1.1.1.tgz";
        sha1 = "9cf1611ba62685d7030ae9e4ba34149c3af03fc8";
      };
    }
    {
      name = "https___registry.npmjs.org_strip_ansi___strip_ansi_3.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_strip_ansi___strip_ansi_3.0.1.tgz";
        url  = "https://registry.npmjs.org/strip-ansi/-/strip-ansi-3.0.1.tgz";
        sha1 = "6a385fb8853d952d5ff05d0e8aaf94278dc63dcf";
      };
    }
    {
      name = "https___registry.npmjs.org_strip_ansi___strip_ansi_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_strip_ansi___strip_ansi_4.0.0.tgz";
        url  = "https://registry.npmjs.org/strip-ansi/-/strip-ansi-4.0.0.tgz";
        sha1 = "a8479022eb1ac368a871389b635262c505ee368f";
      };
    }
    {
      name = "https___registry.npmjs.org_strip_ansi___strip_ansi_5.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_strip_ansi___strip_ansi_5.2.0.tgz";
        url  = "https://registry.npmjs.org/strip-ansi/-/strip-ansi-5.2.0.tgz";
        sha1 = "8c9a536feb6afc962bdfa5b104a5091c1ad9c0ae";
      };
    }
    {
      name = "https___registry.npmjs.org_strip_ansi___strip_ansi_6.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_strip_ansi___strip_ansi_6.0.0.tgz";
        url  = "https://registry.npmjs.org/strip-ansi/-/strip-ansi-6.0.0.tgz";
        sha1 = "0b1571dd7669ccd4f3e06e14ef1eed26225ae532";
      };
    }
    {
      name = "https___registry.npmjs.org_strip_bom___strip_bom_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_strip_bom___strip_bom_4.0.0.tgz";
        url  = "https://registry.npmjs.org/strip-bom/-/strip-bom-4.0.0.tgz";
        sha1 = "9c3505c1db45bcedca3d9cf7a16f5c5aa3901878";
      };
    }
    {
      name = "https___registry.npmjs.org_strip_eof___strip_eof_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_strip_eof___strip_eof_1.0.0.tgz";
        url  = "https://registry.npmjs.org/strip-eof/-/strip-eof-1.0.0.tgz";
        sha1 = "bb43ff5598a6eb05d89b59fcd129c983313606bf";
      };
    }
    {
      name = "https___registry.npmjs.org_strip_final_newline___strip_final_newline_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_strip_final_newline___strip_final_newline_2.0.0.tgz";
        url  = "https://registry.npmjs.org/strip-final-newline/-/strip-final-newline-2.0.0.tgz";
        sha1 = "89b852fb2fcbe936f6f4b3187afb0a12c1ab58ad";
      };
    }
    {
      name = "https___registry.npmjs.org_stylehacks___stylehacks_4.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_stylehacks___stylehacks_4.0.3.tgz";
        url  = "https://registry.npmjs.org/stylehacks/-/stylehacks-4.0.3.tgz";
        sha1 = "6718fcaf4d1e07d8a1318690881e8d96726a71d5";
      };
    }
    {
      name = "https___registry.npmjs.org_supports_color___supports_color_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_supports_color___supports_color_2.0.0.tgz";
        url  = "https://registry.npmjs.org/supports-color/-/supports-color-2.0.0.tgz";
        sha1 = "535d045ce6b6363fa40117084629995e9df324c7";
      };
    }
    {
      name = "https___registry.npmjs.org_supports_color___supports_color_3.2.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_supports_color___supports_color_3.2.3.tgz";
        url  = "https://registry.npmjs.org/supports-color/-/supports-color-3.2.3.tgz";
        sha1 = "65ac0504b3954171d8a64946b2ae3cbb8a5f54f6";
      };
    }
    {
      name = "https___registry.npmjs.org_supports_color___supports_color_5.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_supports_color___supports_color_5.5.0.tgz";
        url  = "https://registry.npmjs.org/supports-color/-/supports-color-5.5.0.tgz";
        sha1 = "e2e69a44ac8772f78a1ec0b35b689df6530efc8f";
      };
    }
    {
      name = "https___registry.npmjs.org_supports_color___supports_color_6.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_supports_color___supports_color_6.1.0.tgz";
        url  = "https://registry.npmjs.org/supports-color/-/supports-color-6.1.0.tgz";
        sha1 = "0764abc69c63d5ac842dd4867e8d025e880df8f3";
      };
    }
    {
      name = "https___registry.npmjs.org_supports_color___supports_color_7.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_supports_color___supports_color_7.2.0.tgz";
        url  = "https://registry.npmjs.org/supports-color/-/supports-color-7.2.0.tgz";
        sha1 = "1b7dcdcb32b8138801b3e478ba6a51caa89648da";
      };
    }
    {
      name = "https___registry.npmjs.org_supports_hyperlinks___supports_hyperlinks_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_supports_hyperlinks___supports_hyperlinks_2.1.0.tgz";
        url  = "https://registry.npmjs.org/supports-hyperlinks/-/supports-hyperlinks-2.1.0.tgz";
        sha1 = "f663df252af5f37c5d49bbd7eeefa9e0b9e59e47";
      };
    }
    {
      name = "https___registry.npmjs.org_svgo___svgo_1.3.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_svgo___svgo_1.3.2.tgz";
        url  = "https://registry.npmjs.org/svgo/-/svgo-1.3.2.tgz";
        sha1 = "b6dc511c063346c9e415b81e43401145b96d4167";
      };
    }
    {
      name = "https___registry.npmjs.org_symbol_tree___symbol_tree_3.2.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_symbol_tree___symbol_tree_3.2.4.tgz";
        url  = "https://registry.npmjs.org/symbol-tree/-/symbol-tree-3.2.4.tgz";
        sha1 = "430637d248ba77e078883951fb9aa0eed7c63fa2";
      };
    }
    {
      name = "https___registry.npmjs.org_terminal_link___terminal_link_2.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_terminal_link___terminal_link_2.1.1.tgz";
        url  = "https://registry.npmjs.org/terminal-link/-/terminal-link-2.1.1.tgz";
        sha1 = "14a64a27ab3c0df933ea546fba55f2d078edc994";
      };
    }
    {
      name = "https___registry.npmjs.org_terser___terser_3.17.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_terser___terser_3.17.0.tgz";
        url  = "https://registry.npmjs.org/terser/-/terser-3.17.0.tgz";
        sha1 = "f88ffbeda0deb5637f9d24b0da66f4e15ab10cb2";
      };
    }
    {
      name = "https___registry.npmjs.org_terser___terser_4.8.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_terser___terser_4.8.0.tgz";
        url  = "https://registry.npmjs.org/terser/-/terser-4.8.0.tgz";
        sha1 = "63056343d7c70bb29f3af665865a46fe03a0df17";
      };
    }
    {
      name = "https___registry.npmjs.org_test_exclude___test_exclude_6.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_test_exclude___test_exclude_6.0.0.tgz";
        url  = "https://registry.npmjs.org/test-exclude/-/test-exclude-6.0.0.tgz";
        sha1 = "04a8698661d805ea6fa293b6cb9e63ac044ef15e";
      };
    }
    {
      name = "https___registry.npmjs.org_throat___throat_5.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_throat___throat_5.0.0.tgz";
        url  = "https://registry.npmjs.org/throat/-/throat-5.0.0.tgz";
        sha1 = "c5199235803aad18754a667d659b5e72ce16764b";
      };
    }
    {
      name = "https___registry.npmjs.org_through2___through2_2.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_through2___through2_2.0.5.tgz";
        url  = "https://registry.npmjs.org/through2/-/through2-2.0.5.tgz";
        sha1 = "01c1e39eb31d07cb7d03a96a70823260b23132cd";
      };
    }
    {
      name = "https___registry.npmjs.org_timers_browserify___timers_browserify_2.0.12.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_timers_browserify___timers_browserify_2.0.12.tgz";
        url  = "https://registry.npmjs.org/timers-browserify/-/timers-browserify-2.0.12.tgz";
        sha1 = "44a45c11fbf407f34f97bccd1577c652361b00ee";
      };
    }
    {
      name = "https___registry.npmjs.org_timsort___timsort_0.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_timsort___timsort_0.3.0.tgz";
        url  = "https://registry.npmjs.org/timsort/-/timsort-0.3.0.tgz";
        sha1 = "405411a8e7e6339fe64db9a234de11dc31e02bd4";
      };
    }
    {
      name = "https___registry.npmjs.org_tiny_inflate___tiny_inflate_1.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_tiny_inflate___tiny_inflate_1.0.3.tgz";
        url  = "https://registry.npmjs.org/tiny-inflate/-/tiny-inflate-1.0.3.tgz";
        sha1 = "122715494913a1805166aaf7c93467933eea26c4";
      };
    }
    {
      name = "https___registry.npmjs.org_tmpl___tmpl_1.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_tmpl___tmpl_1.0.4.tgz";
        url  = "https://registry.npmjs.org/tmpl/-/tmpl-1.0.4.tgz";
        sha1 = "23640dd7b42d00433911140820e5cf440e521dd1";
      };
    }
    {
      name = "https___registry.npmjs.org_to_arraybuffer___to_arraybuffer_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_to_arraybuffer___to_arraybuffer_1.0.1.tgz";
        url  = "https://registry.npmjs.org/to-arraybuffer/-/to-arraybuffer-1.0.1.tgz";
        sha1 = "7d229b1fcc637e466ca081180836a7aabff83f43";
      };
    }
    {
      name = "https___registry.npmjs.org_to_fast_properties___to_fast_properties_1.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_to_fast_properties___to_fast_properties_1.0.3.tgz";
        url  = "https://registry.npmjs.org/to-fast-properties/-/to-fast-properties-1.0.3.tgz";
        sha1 = "b83571fa4d8c25b82e231b06e3a3055de4ca1a47";
      };
    }
    {
      name = "https___registry.npmjs.org_to_fast_properties___to_fast_properties_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_to_fast_properties___to_fast_properties_2.0.0.tgz";
        url  = "https://registry.npmjs.org/to-fast-properties/-/to-fast-properties-2.0.0.tgz";
        sha1 = "dc5e698cbd079265bc73e0377681a4e4e83f616e";
      };
    }
    {
      name = "https___registry.npmjs.org_to_object_path___to_object_path_0.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_to_object_path___to_object_path_0.3.0.tgz";
        url  = "https://registry.npmjs.org/to-object-path/-/to-object-path-0.3.0.tgz";
        sha1 = "297588b7b0e7e0ac08e04e672f85c1f4999e17af";
      };
    }
    {
      name = "https___registry.npmjs.org_to_regex_range___to_regex_range_2.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_to_regex_range___to_regex_range_2.1.1.tgz";
        url  = "https://registry.npmjs.org/to-regex-range/-/to-regex-range-2.1.1.tgz";
        sha1 = "7c80c17b9dfebe599e27367e0d4dd5590141db38";
      };
    }
    {
      name = "https___registry.npmjs.org_to_regex_range___to_regex_range_5.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_to_regex_range___to_regex_range_5.0.1.tgz";
        url  = "https://registry.npmjs.org/to-regex-range/-/to-regex-range-5.0.1.tgz";
        sha1 = "1648c44aae7c8d988a326018ed72f5b4dd0392e4";
      };
    }
    {
      name = "https___registry.npmjs.org_to_regex___to_regex_3.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_to_regex___to_regex_3.0.2.tgz";
        url  = "https://registry.npmjs.org/to-regex/-/to-regex-3.0.2.tgz";
        sha1 = "13cfdd9b336552f30b51f33a8ae1b42a7a7599ce";
      };
    }
    {
      name = "https___registry.npmjs.org_toidentifier___toidentifier_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_toidentifier___toidentifier_1.0.0.tgz";
        url  = "https://registry.npmjs.org/toidentifier/-/toidentifier-1.0.0.tgz";
        sha1 = "7e1be3470f1e77948bc43d94a3c8f4d7752ba553";
      };
    }
    {
      name = "https___registry.npmjs.org_tough_cookie___tough_cookie_2.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_tough_cookie___tough_cookie_2.5.0.tgz";
        url  = "https://registry.npmjs.org/tough-cookie/-/tough-cookie-2.5.0.tgz";
        sha1 = "cd9fb2a0aa1d5a12b473bd9fb96fa3dcff65ade2";
      };
    }
    {
      name = "https___registry.npmjs.org_tough_cookie___tough_cookie_3.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_tough_cookie___tough_cookie_3.0.1.tgz";
        url  = "https://registry.npmjs.org/tough-cookie/-/tough-cookie-3.0.1.tgz";
        sha1 = "9df4f57e739c26930a018184887f4adb7dca73b2";
      };
    }
    {
      name = "https___registry.npmjs.org_tr46___tr46_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_tr46___tr46_1.0.1.tgz";
        url  = "https://registry.npmjs.org/tr46/-/tr46-1.0.1.tgz";
        sha1 = "a8b13fd6bfd2489519674ccde55ba3693b706d09";
      };
    }
    {
      name = "https___registry.npmjs.org_ts_jest___ts_jest_25.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ts_jest___ts_jest_25.5.1.tgz";
        url  = "https://registry.npmjs.org/ts-jest/-/ts-jest-25.5.1.tgz";
        sha1 = "2913afd08f28385d54f2f4e828be4d261f4337c7";
      };
    }
    {
      name = "https___registry.npmjs.org_tty_browserify___tty_browserify_0.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_tty_browserify___tty_browserify_0.0.0.tgz";
        url  = "https://registry.npmjs.org/tty-browserify/-/tty-browserify-0.0.0.tgz";
        sha1 = "a157ba402da24e9bf957f9aa69d524eed42901a6";
      };
    }
    {
      name = "https___registry.npmjs.org_tunnel_agent___tunnel_agent_0.6.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_tunnel_agent___tunnel_agent_0.6.0.tgz";
        url  = "https://registry.npmjs.org/tunnel-agent/-/tunnel-agent-0.6.0.tgz";
        sha1 = "27a5dea06b36b04a0a9966774b290868f0fc40fd";
      };
    }
    {
      name = "https___registry.npmjs.org_tweetnacl___tweetnacl_0.14.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_tweetnacl___tweetnacl_0.14.5.tgz";
        url  = "https://registry.npmjs.org/tweetnacl/-/tweetnacl-0.14.5.tgz";
        sha1 = "5ae68177f192d4456269d108afa93ff8743f4f64";
      };
    }
    {
      name = "https___registry.npmjs.org_type_check___type_check_0.3.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_type_check___type_check_0.3.2.tgz";
        url  = "https://registry.npmjs.org/type-check/-/type-check-0.3.2.tgz";
        sha1 = "5884cab512cf1d355e3fb784f30804b2b520db72";
      };
    }
    {
      name = "https___registry.npmjs.org_type_detect___type_detect_4.0.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_type_detect___type_detect_4.0.8.tgz";
        url  = "https://registry.npmjs.org/type-detect/-/type-detect-4.0.8.tgz";
        sha1 = "7646fb5f18871cfbb7749e69bd39a6388eb7450c";
      };
    }
    {
      name = "https___registry.npmjs.org_type_fest___type_fest_0.11.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_type_fest___type_fest_0.11.0.tgz";
        url  = "https://registry.npmjs.org/type-fest/-/type-fest-0.11.0.tgz";
        sha1 = "97abf0872310fed88a5c466b25681576145e33f1";
      };
    }
    {
      name = "https___registry.npmjs.org_type_fest___type_fest_0.6.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_type_fest___type_fest_0.6.0.tgz";
        url  = "https://registry.npmjs.org/type-fest/-/type-fest-0.6.0.tgz";
        sha1 = "8d2a2370d3df886eb5c90ada1c5bf6188acf838b";
      };
    }
    {
      name = "https___registry.npmjs.org_type_fest___type_fest_0.8.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_type_fest___type_fest_0.8.1.tgz";
        url  = "https://registry.npmjs.org/type-fest/-/type-fest-0.8.1.tgz";
        sha1 = "09e249ebde851d3b1e48d27c105444667f17b83d";
      };
    }
    {
      name = "https___registry.npmjs.org_typedarray_to_buffer___typedarray_to_buffer_3.1.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_typedarray_to_buffer___typedarray_to_buffer_3.1.5.tgz";
        url  = "https://registry.npmjs.org/typedarray-to-buffer/-/typedarray-to-buffer-3.1.5.tgz";
        sha1 = "a97ee7a9ff42691b9f783ff1bc5112fe3fca9080";
      };
    }
    {
      name = "https___registry.npmjs.org_typedarray___typedarray_0.0.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_typedarray___typedarray_0.0.6.tgz";
        url  = "https://registry.npmjs.org/typedarray/-/typedarray-0.0.6.tgz";
        sha1 = "867ac74e3864187b1d3d47d996a78ec5c8830777";
      };
    }
    {
      name = "https___registry.npmjs.org_typescript___typescript_4.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_typescript___typescript_4.1.2.tgz";
        url  = "https://registry.npmjs.org/typescript/-/typescript-4.1.2.tgz";
        sha1 = "6369ef22516fe5e10304aae5a5c4862db55380e9";
      };
    }
    {
      name = "https___registry.npmjs.org_uncss___uncss_0.17.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_uncss___uncss_0.17.3.tgz";
        url  = "https://registry.npmjs.org/uncss/-/uncss-0.17.3.tgz";
        sha1 = "50fc1eb4ed573ffff763458d801cd86e4d69ea11";
      };
    }
    {
      name = "https___registry.npmjs.org_unicode_canonical_property_names_ecmascript___unicode_canonical_property_names_ecmascript_1.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_unicode_canonical_property_names_ecmascript___unicode_canonical_property_names_ecmascript_1.0.4.tgz";
        url  = "https://registry.npmjs.org/unicode-canonical-property-names-ecmascript/-/unicode-canonical-property-names-ecmascript-1.0.4.tgz";
        sha1 = "2619800c4c825800efdd8343af7dd9933cbe2818";
      };
    }
    {
      name = "https___registry.npmjs.org_unicode_match_property_ecmascript___unicode_match_property_ecmascript_1.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_unicode_match_property_ecmascript___unicode_match_property_ecmascript_1.0.4.tgz";
        url  = "https://registry.npmjs.org/unicode-match-property-ecmascript/-/unicode-match-property-ecmascript-1.0.4.tgz";
        sha1 = "8ed2a32569961bce9227d09cd3ffbb8fed5f020c";
      };
    }
    {
      name = "https___registry.npmjs.org_unicode_match_property_value_ecmascript___unicode_match_property_value_ecmascript_1.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_unicode_match_property_value_ecmascript___unicode_match_property_value_ecmascript_1.2.0.tgz";
        url  = "https://registry.npmjs.org/unicode-match-property-value-ecmascript/-/unicode-match-property-value-ecmascript-1.2.0.tgz";
        sha1 = "0d91f600eeeb3096aa962b1d6fc88876e64ea531";
      };
    }
    {
      name = "https___registry.npmjs.org_unicode_property_aliases_ecmascript___unicode_property_aliases_ecmascript_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_unicode_property_aliases_ecmascript___unicode_property_aliases_ecmascript_1.1.0.tgz";
        url  = "https://registry.npmjs.org/unicode-property-aliases-ecmascript/-/unicode-property-aliases-ecmascript-1.1.0.tgz";
        sha1 = "dd57a99f6207bedff4628abefb94c50db941c8f4";
      };
    }
    {
      name = "https___registry.npmjs.org_unicode_trie___unicode_trie_0.3.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_unicode_trie___unicode_trie_0.3.1.tgz";
        url  = "https://registry.npmjs.org/unicode-trie/-/unicode-trie-0.3.1.tgz";
        sha1 = "d671dddd89101a08bac37b6a5161010602052085";
      };
    }
    {
      name = "https___registry.npmjs.org_union_value___union_value_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_union_value___union_value_1.0.1.tgz";
        url  = "https://registry.npmjs.org/union-value/-/union-value-1.0.1.tgz";
        sha1 = "0b6fe7b835aecda61c6ea4d4f02c14221e109847";
      };
    }
    {
      name = "https___registry.npmjs.org_uniq___uniq_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_uniq___uniq_1.0.1.tgz";
        url  = "https://registry.npmjs.org/uniq/-/uniq-1.0.1.tgz";
        sha1 = "b31c5ae8254844a3a8281541ce2b04b865a734ff";
      };
    }
    {
      name = "https___registry.npmjs.org_uniqs___uniqs_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_uniqs___uniqs_2.0.0.tgz";
        url  = "https://registry.npmjs.org/uniqs/-/uniqs-2.0.0.tgz";
        sha1 = "ffede4b36b25290696e6e165d4a59edb998e6b02";
      };
    }
    {
      name = "https___registry.npmjs.org_unquote___unquote_1.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_unquote___unquote_1.1.1.tgz";
        url  = "https://registry.npmjs.org/unquote/-/unquote-1.1.1.tgz";
        sha1 = "8fded7324ec6e88a0ff8b905e7c098cdc086d544";
      };
    }
    {
      name = "https___registry.npmjs.org_unset_value___unset_value_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_unset_value___unset_value_1.0.0.tgz";
        url  = "https://registry.npmjs.org/unset-value/-/unset-value-1.0.0.tgz";
        sha1 = "8376873f7d2335179ffb1e6fc3a8ed0dfc8ab559";
      };
    }
    {
      name = "https___registry.npmjs.org_upath___upath_1.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_upath___upath_1.2.0.tgz";
        url  = "https://registry.npmjs.org/upath/-/upath-1.2.0.tgz";
        sha1 = "8f66dbcd55a883acdae4408af8b035a5044c1894";
      };
    }
    {
      name = "https___registry.npmjs.org_uri_js___uri_js_4.4.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_uri_js___uri_js_4.4.0.tgz";
        url  = "https://registry.npmjs.org/uri-js/-/uri-js-4.4.0.tgz";
        sha1 = "aa714261de793e8a82347a7bcc9ce74e86f28602";
      };
    }
    {
      name = "https___registry.npmjs.org_urix___urix_0.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_urix___urix_0.1.0.tgz";
        url  = "https://registry.npmjs.org/urix/-/urix-0.1.0.tgz";
        sha1 = "da937f7a62e21fec1fd18d49b35c2935067a6c72";
      };
    }
    {
      name = "https___registry.npmjs.org_url___url_0.11.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_url___url_0.11.0.tgz";
        url  = "https://registry.npmjs.org/url/-/url-0.11.0.tgz";
        sha1 = "3838e97cfc60521eb73c525a8e55bfdd9e2e28f1";
      };
    }
    {
      name = "https___registry.npmjs.org_use___use_3.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_use___use_3.1.1.tgz";
        url  = "https://registry.npmjs.org/use/-/use-3.1.1.tgz";
        sha1 = "d50c8cac79a19fbc20f2911f56eb973f4e10070f";
      };
    }
    {
      name = "https___registry.npmjs.org_util_deprecate___util_deprecate_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_util_deprecate___util_deprecate_1.0.2.tgz";
        url  = "https://registry.npmjs.org/util-deprecate/-/util-deprecate-1.0.2.tgz";
        sha1 = "450d4dc9fa70de732762fbd2d4a28981419a0ccf";
      };
    }
    {
      name = "https___registry.npmjs.org_util.promisify___util.promisify_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_util.promisify___util.promisify_1.0.1.tgz";
        url  = "https://registry.npmjs.org/util.promisify/-/util.promisify-1.0.1.tgz";
        sha1 = "6baf7774b80eeb0f7520d8b81d07982a59abbaee";
      };
    }
    {
      name = "https___registry.npmjs.org_util___util_0.10.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_util___util_0.10.3.tgz";
        url  = "https://registry.npmjs.org/util/-/util-0.10.3.tgz";
        sha1 = "7afb1afe50805246489e3db7fe0ed379336ac0f9";
      };
    }
    {
      name = "util___util_0.10.4.tgz";
      path = fetchurl {
        name = "util___util_0.10.4.tgz";
        url  = "https://registry.yarnpkg.com/util/-/util-0.10.4.tgz";
        sha1 = "3aa0125bfe668a4672de58857d3ace27ecb76901";
      };
    }
    {
      name = "https___registry.npmjs.org_util___util_0.11.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_util___util_0.11.1.tgz";
        url  = "https://registry.npmjs.org/util/-/util-0.11.1.tgz";
        sha1 = "3236733720ec64bb27f6e26f421aaa2e1b588d61";
      };
    }
    {
      name = "https___registry.npmjs.org_uuid___uuid_3.4.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_uuid___uuid_3.4.0.tgz";
        url  = "https://registry.npmjs.org/uuid/-/uuid-3.4.0.tgz";
        sha1 = "b23e4358afa8a202fe7a100af1f5f883f02007ee";
      };
    }
    {
      name = "https___registry.npmjs.org_v8_compile_cache___v8_compile_cache_2.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_v8_compile_cache___v8_compile_cache_2.2.0.tgz";
        url  = "https://registry.npmjs.org/v8-compile-cache/-/v8-compile-cache-2.2.0.tgz";
        sha1 = "9471efa3ef9128d2f7c6a7ca39c4dd6b5055b132";
      };
    }
    {
      name = "https___registry.npmjs.org_v8_to_istanbul___v8_to_istanbul_4.1.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_v8_to_istanbul___v8_to_istanbul_4.1.4.tgz";
        url  = "https://registry.npmjs.org/v8-to-istanbul/-/v8-to-istanbul-4.1.4.tgz";
        sha1 = "b97936f21c0e2d9996d4985e5c5156e9d4e49cd6";
      };
    }
    {
      name = "https___registry.npmjs.org_validate_npm_package_license___validate_npm_package_license_3.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_validate_npm_package_license___validate_npm_package_license_3.0.4.tgz";
        url  = "https://registry.npmjs.org/validate-npm-package-license/-/validate-npm-package-license-3.0.4.tgz";
        sha1 = "fc91f6b9c7ba15c857f4cb2c5defeec39d4f410a";
      };
    }
    {
      name = "https___registry.npmjs.org_vendors___vendors_1.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_vendors___vendors_1.0.4.tgz";
        url  = "https://registry.npmjs.org/vendors/-/vendors-1.0.4.tgz";
        sha1 = "e2b800a53e7a29b93506c3cf41100d16c4c4ad8e";
      };
    }
    {
      name = "https___registry.npmjs.org_verror___verror_1.10.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_verror___verror_1.10.0.tgz";
        url  = "https://registry.npmjs.org/verror/-/verror-1.10.0.tgz";
        sha1 = "3a105ca17053af55d6e270c1f8288682e18da400";
      };
    }
    {
      name = "https___registry.npmjs.org_vlq___vlq_0.2.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_vlq___vlq_0.2.3.tgz";
        url  = "https://registry.npmjs.org/vlq/-/vlq-0.2.3.tgz";
        sha1 = "8f3e4328cf63b1540c0d67e1b2778386f8975b26";
      };
    }
    {
      name = "https___registry.npmjs.org_vm_browserify___vm_browserify_1.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_vm_browserify___vm_browserify_1.1.2.tgz";
        url  = "https://registry.npmjs.org/vm-browserify/-/vm-browserify-1.1.2.tgz";
        sha1 = "78641c488b8e6ca91a75f511e7a3b32a86e5dda0";
      };
    }
    {
      name = "https___registry.npmjs.org_w3c_hr_time___w3c_hr_time_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_w3c_hr_time___w3c_hr_time_1.0.2.tgz";
        url  = "https://registry.npmjs.org/w3c-hr-time/-/w3c-hr-time-1.0.2.tgz";
        sha1 = "0a89cdf5cc15822df9c360543676963e0cc308cd";
      };
    }
    {
      name = "https___registry.npmjs.org_w3c_xmlserializer___w3c_xmlserializer_1.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_w3c_xmlserializer___w3c_xmlserializer_1.1.2.tgz";
        url  = "https://registry.npmjs.org/w3c-xmlserializer/-/w3c-xmlserializer-1.1.2.tgz";
        sha1 = "30485ca7d70a6fd052420a3d12fd90e6339ce794";
      };
    }
    {
      name = "https___registry.npmjs.org_walker___walker_1.0.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_walker___walker_1.0.7.tgz";
        url  = "https://registry.npmjs.org/walker/-/walker-1.0.7.tgz";
        sha1 = "2f7f9b8fd10d677262b18a884e28d19618e028fb";
      };
    }
    {
      name = "https___registry.npmjs.org_wcwidth___wcwidth_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_wcwidth___wcwidth_1.0.1.tgz";
        url  = "https://registry.npmjs.org/wcwidth/-/wcwidth-1.0.1.tgz";
        sha1 = "f0b0dcf915bc5ff1528afadb2c0e17b532da2fe8";
      };
    }
    {
      name = "https___registry.npmjs.org_webidl_conversions___webidl_conversions_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_webidl_conversions___webidl_conversions_4.0.2.tgz";
        url  = "https://registry.npmjs.org/webidl-conversions/-/webidl-conversions-4.0.2.tgz";
        sha1 = "a855980b1f0b6b359ba1d5d9fb39ae941faa63ad";
      };
    }
    {
      name = "https___registry.npmjs.org_whatwg_encoding___whatwg_encoding_1.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_whatwg_encoding___whatwg_encoding_1.0.5.tgz";
        url  = "https://registry.npmjs.org/whatwg-encoding/-/whatwg-encoding-1.0.5.tgz";
        sha1 = "5abacf777c32166a51d085d6b4f3e7d27113ddb0";
      };
    }
    {
      name = "https___registry.npmjs.org_whatwg_mimetype___whatwg_mimetype_2.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_whatwg_mimetype___whatwg_mimetype_2.3.0.tgz";
        url  = "https://registry.npmjs.org/whatwg-mimetype/-/whatwg-mimetype-2.3.0.tgz";
        sha1 = "3d4b1e0312d2079879f826aff18dbeeca5960fbf";
      };
    }
    {
      name = "https___registry.npmjs.org_whatwg_url___whatwg_url_7.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_whatwg_url___whatwg_url_7.1.0.tgz";
        url  = "https://registry.npmjs.org/whatwg-url/-/whatwg-url-7.1.0.tgz";
        sha1 = "c2c492f1eca612988efd3d2266be1b9fc6170d06";
      };
    }
    {
      name = "https___registry.npmjs.org_which_module___which_module_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_which_module___which_module_2.0.0.tgz";
        url  = "https://registry.npmjs.org/which-module/-/which-module-2.0.0.tgz";
        sha1 = "d9ef07dce77b9902b8a3a8fa4b31c3e3f7e6e87a";
      };
    }
    {
      name = "https___registry.npmjs.org_which___which_1.3.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_which___which_1.3.1.tgz";
        url  = "https://registry.npmjs.org/which/-/which-1.3.1.tgz";
        sha1 = "a45043d54f5805316da8d62f9f50918d3da70b0a";
      };
    }
    {
      name = "https___registry.npmjs.org_which___which_2.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_which___which_2.0.2.tgz";
        url  = "https://registry.npmjs.org/which/-/which-2.0.2.tgz";
        sha1 = "7c6a8dd0a636a0327e10b59c9286eee93f3f51b1";
      };
    }
    {
      name = "https___registry.npmjs.org_word_wrap___word_wrap_1.2.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_word_wrap___word_wrap_1.2.3.tgz";
        url  = "https://registry.npmjs.org/word-wrap/-/word-wrap-1.2.3.tgz";
        sha1 = "610636f6b1f703891bd34771ccb17fb93b47079c";
      };
    }
    {
      name = "https___registry.npmjs.org_wrap_ansi___wrap_ansi_6.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_wrap_ansi___wrap_ansi_6.2.0.tgz";
        url  = "https://registry.npmjs.org/wrap-ansi/-/wrap-ansi-6.2.0.tgz";
        sha1 = "e9393ba07102e6c91a3b221478f0257cd2856e53";
      };
    }
    {
      name = "https___registry.npmjs.org_wrappy___wrappy_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_wrappy___wrappy_1.0.2.tgz";
        url  = "https://registry.npmjs.org/wrappy/-/wrappy-1.0.2.tgz";
        sha1 = "b5243d8f3ec1aa35f1364605bc0d1036e30ab69f";
      };
    }
    {
      name = "https___registry.npmjs.org_write_file_atomic___write_file_atomic_3.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_write_file_atomic___write_file_atomic_3.0.3.tgz";
        url  = "https://registry.npmjs.org/write-file-atomic/-/write-file-atomic-3.0.3.tgz";
        sha1 = "56bd5c5a5c70481cd19c571bd39ab965a5de56e8";
      };
    }
    {
      name = "https___registry.npmjs.org_ws___ws_5.2.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ws___ws_5.2.2.tgz";
        url  = "https://registry.npmjs.org/ws/-/ws-5.2.2.tgz";
        sha1 = "dffef14866b8e8dc9133582514d1befaf96e980f";
      };
    }
    {
      name = "https___registry.npmjs.org_ws___ws_6.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ws___ws_6.2.1.tgz";
        url  = "https://registry.npmjs.org/ws/-/ws-6.2.1.tgz";
        sha1 = "442fdf0a47ed64f59b6a5d8ff130f4748ed524fb";
      };
    }
    {
      name = "https___registry.npmjs.org_ws___ws_7.4.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ws___ws_7.4.1.tgz";
        url  = "https://registry.npmjs.org/ws/-/ws-7.4.1.tgz";
        sha1 = "a333be02696bd0e54cea0434e21dcc8a9ac294bb";
      };
    }
    {
      name = "https___registry.npmjs.org_xml_name_validator___xml_name_validator_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_xml_name_validator___xml_name_validator_3.0.0.tgz";
        url  = "https://registry.npmjs.org/xml-name-validator/-/xml-name-validator-3.0.0.tgz";
        sha1 = "6ae73e06de4d8c6e47f9fb181f78d648ad457c6a";
      };
    }
    {
      name = "https___registry.npmjs.org_xmlchars___xmlchars_2.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_xmlchars___xmlchars_2.2.0.tgz";
        url  = "https://registry.npmjs.org/xmlchars/-/xmlchars-2.2.0.tgz";
        sha1 = "060fe1bcb7f9c76fe2a17db86a9bc3ab894210cb";
      };
    }
    {
      name = "https___registry.npmjs.org_xtend___xtend_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_xtend___xtend_4.0.2.tgz";
        url  = "https://registry.npmjs.org/xtend/-/xtend-4.0.2.tgz";
        sha1 = "bb72779f5fa465186b1f438f674fa347fdb5db54";
      };
    }
    {
      name = "https___registry.npmjs.org_y18n___y18n_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_y18n___y18n_4.0.1.tgz";
        url  = "https://registry.npmjs.org/y18n/-/y18n-4.0.1.tgz";
        sha1 = "8db2b83c31c5d75099bb890b23f3094891e247d4";
      };
    }
    {
      name = "https___registry.npmjs.org_yargs_parser___yargs_parser_18.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_yargs_parser___yargs_parser_18.1.3.tgz";
        url  = "https://registry.npmjs.org/yargs-parser/-/yargs-parser-18.1.3.tgz";
        sha1 = "be68c4975c6b2abf469236b0c870362fab09a7b0";
      };
    }
    {
      name = "https___registry.npmjs.org_yargs___yargs_15.4.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_yargs___yargs_15.4.1.tgz";
        url  = "https://registry.npmjs.org/yargs/-/yargs-15.4.1.tgz";
        sha1 = "0d87a16de01aee9d8bec2bfbf74f67851730f4f8";
      };
    }
  ];
}
