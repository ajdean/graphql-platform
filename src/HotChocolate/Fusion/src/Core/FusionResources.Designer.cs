﻿//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

namespace HotChocolate.Fusion {
    using System;


    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Resources.Tools.StronglyTypedResourceBuilder", "4.0.0.0")]
    [System.Diagnostics.DebuggerNonUserCodeAttribute()]
    [System.Runtime.CompilerServices.CompilerGeneratedAttribute()]
    internal class FusionResources {

        private static System.Resources.ResourceManager resourceMan;

        private static System.Globalization.CultureInfo resourceCulture;

        [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
        internal FusionResources() {
        }

        [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
        internal static System.Resources.ResourceManager ResourceManager {
            get {
                if (object.Equals(null, resourceMan)) {
                    System.Resources.ResourceManager temp = new System.Resources.ResourceManager("HotChocolate.Fusion.FusionResources", typeof(FusionResources).Assembly);
                    resourceMan = temp;
                }
                return resourceMan;
            }
        }

        [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
        internal static System.Globalization.CultureInfo Culture {
            get {
                return resourceCulture;
            }
            set {
                resourceCulture = value;
            }
        }

        internal static string ThrowHelper_ServiceConfDocumentMustContainSchemaDef {
            get {
                return ResourceManager.GetString("ThrowHelper_ServiceConfDocumentMustContainSchemaDef", resourceCulture);
            }
        }

        internal static string ThrowHelper_ServiceConfNoClientsSpecified {
            get {
                return ResourceManager.GetString("ThrowHelper_ServiceConfNoClientsSpecified", resourceCulture);
            }
        }

        internal static string ThrowHelper_ServiceConfNoTypesSpecified {
            get {
                return ResourceManager.GetString("ThrowHelper_ServiceConfNoTypesSpecified", resourceCulture);
            }
        }

        internal static string ThrowHelper_ServiceConfInvalidValue {
            get {
                return ResourceManager.GetString("ThrowHelper_ServiceConfInvalidValue", resourceCulture);
            }
        }

        internal static string ThrowHelper_ServiceConfInvalidDirectiveName {
            get {
                return ResourceManager.GetString("ThrowHelper_ServiceConfInvalidDirectiveName", resourceCulture);
            }
        }

        internal static string ThrowHelper_ServiceConfNoDirectiveArgs {
            get {
                return ResourceManager.GetString("ThrowHelper_ServiceConfNoDirectiveArgs", resourceCulture);
            }
        }

        internal static string ThrowHelper_ServiceConfInvalidDirectiveArgs {
            get {
                return ResourceManager.GetString("ThrowHelper_ServiceConfInvalidDirectiveArgs", resourceCulture);
            }
        }

        internal static string FusionGraphConfigurationReader_ReadResolverDefinition_InvalidKindValue {
            get {
                return ResourceManager.GetString("FusionGraphConfigurationReader_ReadResolverDefinition_InvalidKindValue", resourceCulture);
            }
        }

        internal static string FusionRequestExecutorBuilderExtensions_AddFusionGatewayServer_NoSchema {
            get {
                return ResourceManager.GetString("FusionRequestExecutorBuilderExtensions_AddFusionGatewayServer_NoSchema", resourceCulture);
            }
        }

        internal static string ThrowHelper_Requirement_Is_Missing {
            get {
                return ResourceManager.GetString("ThrowHelper_Requirement_Is_Missing", resourceCulture);
            }
        }

        internal static string ThrowHelper_NoResolverInContext {
            get {
                return ResourceManager.GetString("ThrowHelper_NoResolverInContext", resourceCulture);
            }
        }

        internal static string TransportConfigurationNotSupported {
            get {
                return ResourceManager.GetString("TransportConfigurationNotSupported", resourceCulture);
            }
        }

        internal static string CreateSelection_MustBePlaceholderOrSelectExpression {
            get {
                return ResourceManager.GetString("CreateSelection_MustBePlaceholderOrSelectExpression", resourceCulture);
            }
        }
    }
}
