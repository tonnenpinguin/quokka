# Copyright 2024 Adobe. All rights reserved.
# Copyright 2025 SmartRent. All rights reserved.
# This file is licensed to you under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License. You may obtain a copy
# of the License at http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software distributed under
# the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR REPRESENTATIONS
# OF ANY KIND, either express or implied. See the License for the specific language
# governing permissions and limitations under the License.

defmodule Quokka.Style.ModuleDirectivesTest do
  @moduledoc false
  use Quokka.StyleCase, async: true
  use Mimic

  setup do
    stub(Quokka.Config, :rewrite_multi_alias?, fn -> true end)
    stub(Quokka.Config, :zero_arity_parens?, fn -> true end)

    stub(Quokka.Config, :strict_module_layout_order, fn ->
      [
        :shortdoc,
        :moduledoc,
        :behaviour,
        :use,
        :import,
        :alias,
        :require
      ]
    end)

    :ok
  end

  describe "skip comment" do
    test "skips module reordering" do
      assert_style("""
      defmodule Foo do
        # quokka:skip-module-reordering
        @behaviour Lawful
        require A
        alias A.{A, B}

        use B

        def c(x), do: y

        import C
        @behaviour Chaotic
        @doc "d doc"
        def d() do
          alias X.X
          alias H.H

          alias Z.Z
          import Ecto.Query
          X.foo()
        end

        @shortdoc "it's pretty short"
        import A
        alias C.C
        alias D.D

        require C
        require B

        use A

        alias C.C
        alias A.A

        @moduledoc "README.md"
                   |> File.read!()
                   |> String.split("<!-- MDOC !-->")
                   |> Enum.fetch!(1)
      end
      """)
    end
  end

  describe "defmodule features" do
    test "handles module with no directives" do
      assert_style("""
      defmodule Test do
        def foo(), do: :ok
      end
      """)
    end

    test "handles dynamically generated modules" do
      assert_style("""
      Enum.each(testing_list, fn test_item ->
        defmodule test_item do
        end
      end)
      """)
    end

    test "module with single child" do
      assert_style(
        """
        defmodule ATest do
          alias Foo.{A, B}
        end
        """,
        """
        defmodule ATest do
          alias Foo.A
          alias Foo.B
        end
        """
      )
    end

    test "adds moduledoc" do
      assert_style(
        """
        defmodule DocsOnly do
          @moduledoc "woohoo"
        end
        """,
        """
        defmodule DocsOnly do
          @moduledoc "woohoo"
        end
        """
      )

      assert_style("""
      defmodule Foo do
        use Bar
      end
      """)

      assert_style(
        """
        defmodule Foo do
          alias Foo.{Bar, Baz}
        end
        """,
        """
        defmodule Foo do
          alias Foo.Bar
          alias Foo.Baz
        end
        """
      )

      assert_style(
        """
        defmodule A do
          defmodule B do
            :literal
          end

        end
        """,
        """
        defmodule A do
          defmodule B do
            :literal
          end
        end
        """
      )
    end

    test "skips keyword defmodules" do
      assert_style("defmodule Foo, do: use(Bar)")
    end

    test "doesn't add moduledoc to modules of specific names" do
      for verboten <- ~w(Test Mixfile Controller Endpoint Repo Router Socket View HTML JSON) do
        assert_style("""
        defmodule A.B.C#{verboten} do
          @shortdoc "Don't change me!"
        end
        """)
      end
    end

    test "groups directives in order" do
      assert_style(
        """
        defmodule Foo do
          @behaviour Lawful
          require A
          alias A.A

          use B

          def c(x), do: y

          import C
          @behaviour Chaotic
          @doc "d doc"
          def d do
            alias X.X
            alias H.H

            alias Z.Z
            import Ecto.Query
            X.foo()
          end
          @shortdoc "it's pretty short"
          import A
          alias C.C
          alias D.D

          require C
          require B

          use A

          alias C.C
          alias A.A

          @moduledoc "README.md"
                     |> File.read!()
                     |> String.split("<!-- MDOC !-->")
                     |> Enum.fetch!(1)
        end
        """,
        """
        defmodule Foo do
          @shortdoc "it's pretty short"
          @moduledoc "README.md"
                     |> File.read!()
                     |> String.split("<!-- MDOC !-->")
                     |> Enum.fetch!(1)
          @behaviour Chaotic
          @behaviour Lawful

          use B
          use A.A

          import A.A
          import C

          alias A.A
          alias C.C
          alias D.D

          require A
          require B
          require C

          def c(x), do: y

          @doc "d doc"
          def d() do
            import Ecto.Query

            alias H.H
            alias X.X
            alias Z.Z

            X.foo()
          end
        end
        """
      )
    end

    test "does not dealias attr directives if not needed" do
      stub(Quokka.Config, :strict_module_layout_order, fn ->
        [
          :alias,
          :behaviour,
          :moduledoc,
          :shortdoc
        ]
      end)

      assert_style("""
      defmodule Foo do
        alias Foo.Bar

        @behaviour Bar

        @moduledoc "This module \#{Bar}"

        @shortdoc "This module \#{Bar}"
      end
      """)
    end

    test "handles custom order with non-existent keys" do
      stub(Quokka.Config, :strict_module_layout_order, fn ->
        [
          :shortdoc,
          :moduledoc,
          :callback,
          :behaviour,
          :use,
          :import,
          :alias,
          :require
        ]
      end)

      assert_style(
        """
        defmodule Foo do
          alias A.A
          use B
          @moduledoc "test"
          @shortdoc "short"
        end
        """,
        """
        defmodule Foo do
          @shortdoc "short"
          @moduledoc "test"
          use B

          alias A.A
        end
        """
      )
    end
  end

  describe "strange parents!" do
    test "regression: only triggers on SpecialForms, ignoring functions and vars" do
      assert_style("def foo(alias), do: Foo.bar(alias)")

      assert_style("""
      defmodule Foo do
        @moduledoc false
        @spec import(any(), any(), any()) :: any()
        def import(a, b, c), do: nil
      end
      """)
    end

    test "anonymous function" do
      assert_style("fn -> alias A.{C, B} end", """
      fn ->
        alias A.B
        alias A.C
      end
      """)
    end

    test "quote do with one child" do
      assert_style(
        """
        quote do
          alias A.{C, B}
        end
        """,
        """
        quote do
          alias A.B
          alias A.C
        end
        """
      )
    end

    test "quote do with multiple children" do
      assert_style("""
      quote do
        import A
        import B
      end
      """)
    end
  end

  describe "directive sort/dedupe/expansion" do
    test "isn't fooled by function names" do
      assert_style(
        """
        def import(foo) do
          import B

          import A
        end
        """,
        """
        def import(foo) do
          import A
          import B
        end
        """
      )
    end

    test "handles a lonely lonely directive" do
      assert_style("import Foo")
    end

    test "sorts, dedupes & expands alias/require/import while respecting groups" do
      for d <- ~w(alias require import) do
        assert_style(
          """
          #{d} D.D
          #{d} A.{B}
          #{d} A.{
            A.A,
            B,
            C
          }
          #{d} A.B

          #{d} B.B
          #{d} A.A
          """,
          """
          #{d} A.A
          #{d} A.A.A
          #{d} A.B
          #{d} A.C
          #{d} B.B
          #{d} D.D
          """
        )
      end
    end

    test "expands __MODULE__" do
      assert_style(
        """
        alias __MODULE__.{B.D, A}
        """,
        """
        alias __MODULE__.A
        alias __MODULE__.B.D
        """
      )
    end

    test "expands use but does not sort it" do
      assert_style(
        """
        use D
        use A
        use A.{
          C,
          B
        }
        import F
        """,
        """
        use D
        use A
        use A.C
        use A.B

        import F
        """
      )
    end

    test "interwoven directives w/o the context of a module" do
      assert_style(
        """
        @type foo :: :ok
        alias D.D
        alias A.{B}
        require A.{
          A,
          C
        }
        alias B.B
        alias A.A
        """,
        """
        alias A.A
        alias A.B
        alias B.B
        alias D.D

        require A.A
        require A.C

        @type foo :: :ok
        """
      )
    end

    test "respects as" do
      assert_style("""
      alias Foo.Asset
      alias Foo.Project.Loaders, as: ProjectLoaders
      alias Foo.ProjectDevice.Loaders, as: ProjectDeviceLoaders
      alias Foo.User.Loaders
      """)
    end

    test "respects rewrite_multi_alias false" do
      stub(Quokka.Config, :rewrite_multi_alias?, fn -> false end)

      assert_style("""
      alias A.{B, C}
      """)

      assert_style(
        """
        alias A.D
        alias A.{B, E, C}
        """,
        """
        alias A.D
        alias A.{B, C, E}
        """
      )
    end

    test "sorts nested alias/import/require braces when not expanded" do
      stub(Quokka.Config, :rewrite_multi_alias?, fn -> false end)

      assert_style(
        """
        alias A.{C, B, E}
        require Bar.{B, A}
        import Foo.{Z, A, M}
        """,
        """
        import Foo.{A, M, Z}

        alias A.{B, C, E}

        require Bar.{A, B}
        """
      )

      assert_style(
        """
        alias A.A
        alias __MODULE__.{C, B.D, B.A, A}
        """,
        """
        alias __MODULE__.{A, B.A, B.D, C}
        alias A.A
        """
      )
    end
  end

  describe "with comments..." do
    test "moving aliases up through non-directives doesn't move comments up" do
      assert_style(
        """
        defmodule Foo do
          # mdf
          @moduledoc false
          # B
          alias B.B

          # foo
          def foo() do
            # ok
            :ok
          end
          # C
          alias C.C
          # A
          alias A.A
        end
        """,
        """
        defmodule Foo do
          # mdf
          @moduledoc false
          alias A.A
          # B
          alias B.B
          alias C.C

          # foo
          def foo() do
            # ok
            :ok
          end

          # C
          # A
        end
        """
      )
    end
  end

  test "Deletes root level alias" do
    assert_style("alias Foo", "")

    assert_style(
      """
      alias Foo

      Foo.bar()
      """,
      "Foo.bar()"
    )

    assert_style(
      """
      alias unquote(Foo)
      alias Foo
      alias Bar, as: Bop
      alias __MODULE__
      """,
      """
      alias __MODULE__
      alias Bar, as: Bop
      alias unquote(Foo)
      """
    )

    assert_style(
      """
      alias A.A
      alias B.B
      alias C

      require D
      """,
      """
      alias A.A
      alias B.B

      require D
      """
    )
  end

  test "@derive movements" do
    assert_style(
      """
      defmodule F do
        @moduledoc "This is a test"
        defstruct [:a]
        # comment for foo
        def foo, do: :ok
        @derive Inspect
        @derive {Foo, bar: :baz}
      end
      """,
      """
      defmodule F do
        @moduledoc "This is a test"
        @derive Inspect
        @derive {Foo, bar: :baz}
        defstruct [:a]
        # comment for foo
        def foo(), do: :ok
      end
      """
    )

    assert_style "@derive Inspect"
  end

  test "de-aliases use/behaviour/import/moduledoc" do
    assert_style(
      """
      defmodule MyModule do
        alias A.B.C
        @moduledoc "Implements \#{C.foo()}!"
        alias D.F.C
        import C
        alias G.H.C
        @behaviour C
        alias Z.X.C
        use SomeMacro, with: C
        alias A.B, as: D
        import D
      end
      """,
      """
      defmodule MyModule do
        @moduledoc "Implements \#{A.B.C.foo()}!"
        @behaviour G.H.C

        use SomeMacro, with: Z.X.C

        import A.B
        import D.F.C

        alias A.B, as: D
        alias A.B.C
        alias D.F.C
        alias G.H.C
        alias Z.X.C
      end
      """
    )
  end

  describe "module attribute lifting" do
    test "replaces uses in other attributes and `use` correctly" do
      assert_style(
        """
        defmodule MyGreatLibrary do
          @library_options [...]
          @moduledoc make_pretty_docs(@library_options)
          use OptionsMagic, my_opts: @library_options
        end
        """,
        """
        library_options = [...]

        defmodule MyGreatLibrary do
          @moduledoc make_pretty_docs(library_options)
          use OptionsMagic, my_opts: unquote(library_options)

          @library_options library_options
        end
        """
      )
    end

    test "works with `quote`" do
      assert_style(
        """
        quote do
          @library_options [...]
          @moduledoc make_pretty_docs(@library_options)
          use OptionsMagic, my_opts: @library_options
        end
        """,
        """
        library_options = [...]

        quote do
          @moduledoc make_pretty_docs(library_options)
          use OptionsMagic, my_opts: unquote(library_options)

          @library_options library_options
        end
        """
      )
    end
  end

  describe "remove unused aliases" do
    setup do
      stub(Quokka.Config, :remove_unused_aliases?, fn -> true end)
      :ok
    end

    test "removes alias that is not used" do
      assert_style(
        """
        defmodule Foo do
          alias Foo.Bar
          alias Baz.Qux

          def foo do
            Bar.baz()
          end
        end
        """,
        """
        defmodule Foo do
          alias Foo.Bar

          def foo() do
            Bar.baz()
          end
        end
        """
      )
    end

    test "handles nested aliases" do
      assert_style(
        """
        defmodule Foo do
          alias Foo.Bar
          alias Unused.Nested.Module

          def foo do
            Bar.Baz.call()
            Bar.Foo.call()
          end
        end
        """,
        """
        defmodule Foo do
          alias Foo.Bar

          def foo() do
            Bar.Baz.call()
            Bar.Foo.call()
          end
        end
        """
      )
    end

    test "keeps alias that is used by short form" do
      assert_style("""
      defmodule Foo do
        alias Foo.Bar

        def foo() do
          Bar.baz()
        end
      end
      """)
    end

    test "removes alias only used in fully qualified name" do
      assert_style(
        """
        defmodule Foo do
          alias Foo.Bar

          def foo do
            Foo.Bar.baz()
          end
        end
        """,
        """
        defmodule Foo do
          def foo() do
            Foo.Bar.baz()
          end
        end
        """
      )
    end

    test "removes multiple unused aliases" do
      assert_style(
        """
        defmodule Foo do
          alias Foo.Bar
          alias Baz.Qux
          alias Hello.World
          alias Used.Module

          def foo do
            Module.do_something()
          end
        end
        """,
        """
        defmodule Foo do
          alias Used.Module

          def foo() do
            Module.do_something()
          end
        end
        """
      )
    end

    test "handles aliases with as: keyword" do
      assert_style(
        """
        defmodule Foo do
          alias Foo.Bar, as: MyBar
          alias Baz.Qux, as: MyQux

          def foo do
            MyBar.baz()
          end
        end
        """,
        """
        defmodule Foo do
          alias Foo.Bar, as: MyBar

          def foo() do
            MyBar.baz()
          end
        end
        """
      )
    end

    test "does not remove aliases if used in quote block" do
      assert_style(
        """
        defmodule Foo do
          alias Foo.Bar

          quote do
            alias Bax.Qux
          end
        end
        """,
        """
        defmodule Foo do
          quote do
            alias Bax.Qux
          end
        end
        """
      )
    end

    test "alias with as: is not used if only short form used (without as)" do
      # The short form Bar is used, but since the alias was `as: MyBar`,
      # the alias is not actually being used, and Bar refers to the full qualified name
      assert_style(
        """
        defmodule Foo do
          alias Foo.Bar, as: MyBar

          def foo do
            MyBar.baz()
          end
        end
        """,
        """
        defmodule Foo do
          alias Foo.Bar, as: MyBar

          def foo() do
            MyBar.baz()
          end
        end
        """
      )
    end

    test "keeps aliases used in multiple places" do
      assert_style("""
      defmodule Foo do
        alias Foo.Bar

        def foo() do
          Bar.baz()
          Bar.qux()
        end

        def bar() do
          Bar.other()
        end
      end
      """)
    end

    test "keeps aliases used in @derive" do
      assert_style("""
      defmodule Foo do
        alias Foo.Encoder

        @derive {Encoder, []}
        defstruct [:a]
      end
      """)
    end

    test "respects configuration flag" do
      stub(Quokka.Config, :remove_unused_aliases?, fn -> false end)

      # When remove_unused_aliases is false, aliases are still sorted but not removed
      assert_style(
        """
        defmodule Foo do
          alias Foo.Bar
          alias Baz.Qux
        end
        """,
        """
        defmodule Foo do
          alias Baz.Qux
          alias Foo.Bar
        end
        """
      )
    end

    test "removes aliases with mixed usage patterns" do
      assert_style(
        """
        defmodule Foo do
          alias Foo.Used
          alias Foo.Unused
          alias Bar.OnlyFullyQualified

          def foo do
            Used.call()
            Bar.OnlyFullyQualified.call()
          end
        end
        """,
        """
        defmodule Foo do
          alias Foo.Used

          def foo() do
            Used.call()
            Bar.OnlyFullyQualified.call()
          end
        end
        """
      )
    end

    test "handles nested modules" do
      assert_style(
        """
        defmodule Foo do
          alias Foo.Bar.Baz
          alias Unused.Nested.Module

          def foo do
            Baz.call()
          end
        end
        """,
        """
        defmodule Foo do
          alias Foo.Bar.Baz

          def foo() do
            Baz.call()
          end
        end
        """
      )
    end

    test "keeps all aliases when all are used" do
      assert_style("""
      defmodule Foo do
        alias Bar.B
        alias Baz.C
        alias Foo.A

        def foo() do
          A.call()
          B.call()
          C.call()
        end
      end
      """)
    end

    test "handles aliases in multiple directive groups" do
      assert_style(
        """
        defmodule Foo do
          alias Used.A
          alias Unused.B
          require Used.C
          alias Unused.D

          def foo do
            A.call()
          end
        end
        """,
        """
        defmodule Foo do
          alias Used.A

          require Used.C

          def foo() do
            A.call()
          end
        end
        """
      )
    end
  end
end
