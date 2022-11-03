defmodule Spdx3.Model.DataTypes do
  defmodule ProfileIdentifier do
    defstruct name: "core"
  end

  defmodule CreationInformation do
    @enforce_keys [:createdBy]
    defstruct [ :createdBy,
                profile: [%ProfileIdentifier{}],
                created: DateTime.utc_now(),
                specVersion: "3.0.0",
                dataLicense: :CC0
              ]
  end
end


defmodule SPDX3.Model do
  @moduledoc """
  Documentation for `SPDX3.Model`.
  """

  defprotocol Element do
    @spec spdxid(t) :: String.t()
    def spdxid(value)
    @spec creationInfo(t) :: any
    def creationInfo(value)
    @spec name(t) :: any
    def name(value)
    # @spec summary(t) :: any
    # def summary(term)
    # @spec description(t) :: any
    # def description(term)
    # @spec comment(t) :: any
    # def comment(term)
    # @spec verifiedUsing(t) :: any
    # def verifiedUsing(term)
    # @spec externalReferences(t) :: any
    # def externalReferences(term)
    # @spec externalIdentifiers(t) :: any
    # def externalIdentifiers(term)
    # @spec extensions(t) :: any
    # def extensions(term)
  end


  defprotocol Actor do
    @behaviour Element

    @behaviour Element
    @spec spdxid(t) :: String.t()
    def spdxid(value)
    @spec creationInfo(t) :: any
    def creationInfo(value)
    @spec name(t) :: any
    def name(value)
  end

  defprotocol Artifact do
    @spec originatedBy(t) :: [Actor]
    def originatedBy(value)

    @behaviour Element
    @spec spdxid(t) :: String.t()
    def spdxid(value)
    @spec creationInfo(t) :: any
    def creationInfo(value)
    @spec name(t) :: any
    def name(value)
  end

  defprotocol Collection do
    # @spec namespaces(t) :: [NamespaceMap]
    # def namespaces(t)
    # @spec imports(t) :: [ExternalMap]
    # def imports (t)
    @spec elements(t) :: [Element]
    def elements(value)
    @spec rootElements(t) :: [Element]
    def rootElements(value)

    @behaviour Element
    @spec spdxid(t) :: String.t()
    def spdxid(value)
    @spec creationInfo(t) :: any
    def creationInfo(value)
    @spec name(t) :: any
    def name(value)
  end

  defprotocol Bundle do
    @spec context(t) :: String
    def context(value)
  end

  defprotocol SpdxDocument do
  end


  defmodule Impl do
    defmodule Element do
      @enforce_keys [:creationInfo]
      defstruct [ :spdxid,
                  :creationInfo,
                  name: nil,
                  summary: nil,
                  description: nil,
                  comment: nil,
                  verifiedUsing: [],
                  externalReferences: [],
                  externalIdentifiers: [],
                  extensions: []
      ]
      defimpl SPDX3.Model.Element do
        def spdxid(%Element{spdxid: spdxid}) do
          spdxid
        end
        def creationInfo(%Element{creationInfo: creationInfo}) do
          creationInfo
        end
        def name(%Element{name: name}) do
          name
        end
      end
    end

    defmodule Artifact do
      defstruct [:element, originatedBy: []]
      defimpl SPDX3.Model.Artifact do
        def originatedBy(%Artifact{originatedBy: originatedBy}) do
          originatedBy
        end

        def spdxid(%Artifact{element: %Element{spdxid: spdxid}}) do
          spdxid
        end
        def creationInfo(%Artifact{element: %Element{creationInfo: creationInfo}}) do
          creationInfo
        end
        def name(%Artifact{element: %Element{name: name}}) do
          name
        end
      end
    end

    defmodule Collection do
      defstruct [:element, elements: [], rootElements: []]
      defimpl SPDX3.Model.Collection do
        def elements(%Collection{elements: elements}) do
          elements
        end
        def rootElements(%Collection{rootElements: rootElements}) do
          rootElements
        end

        def spdxid(%Collection{element: %Element{spdxid: spdxid}}) do
          spdxid
        end
        def creationInfo(%Collection{element: %Element{creationInfo: creationInfo}}) do
          creationInfo
        end
        def name(%Collection{element: %Element{name: name}}) do
          name
        end
      end
    end
  end

  @doc """
    create an exmaple SPDX3 document
  """
  def example do
    creator = "me"
    artifact = %SPDX3.Model.Impl.Artifact{
      element: %SPDX3.Model.Impl.Element{
        spdxid: "urn://uaie/uaie",
        creationInfo: %Spdx3.Model.DataTypes.CreationInformation{
          createdBy: creator
        },
        name: "exampleArtifact"
      }
    }

    %SPDX3.Model.Impl.Collection{
      element: %SPDX3.Model.Impl.Element{
        spdxid: "urn://uaie/uaie",
        creationInfo: %Spdx3.Model.DataTypes.CreationInformation{
          createdBy: creator
        },
        name: "example"
      },
      elements: [artifact],
      rootElements: [artifact]
    }
  end
end
