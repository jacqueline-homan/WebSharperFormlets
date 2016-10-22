namespace Samples

open WebSharper
open WebSharper.Html.Client

module FormletSample =
    open WebSharper.Formlets
    open WebSharper.Formlets.Controls

    module Server =
        [<Rpc>]
        let ProductList () : List<string * int> =
            [
                "Banana", 1
                "Apple", 2
                "Orange", 3
            ]

    [<JavaScript>]
    module Client =
        type ProductQuantity =
            {
                ProductId : int
                Quantity : int
            }

        let ProductFormlet () =
            let products = Server.ProductList ()
            let productF =
                Select 0 products
                |> Enhance.WithTextLabel "Product"
            let quantityF =
                Input ""
                |> Validator.IsInt "Integer value required"
                |> Enhance.WithValidationIcon
                |> Enhance.WithTextLabel "Quantity"
            Formlet.Yield (fun prodId qnt ->
                {ProductId = prodId; Quantity = int qnt})
            <*> productF
            <*> quantityF

        type Address =
            {
                Street : string
                City : string
                Country : string
            }

        let AddressFormlet () : Formlet<Address> =
            let inputF label errMsg =
                Input ""
                |> Validator.IsNotEmpty errMsg
                |> Enhance.WithValidationIcon
                |> Enhance.WithTextLabel label
            Formlet.Yield (fun st ct cnt ->
                {Street = st; City = ct; Country = cnt})
            <*> inputF "Street" "Empty street not allowed"
            <*> inputF "City" "Empty city not allowed"
            <*> inputF "Country" "Empty country not allowed"

        type Contact =
            | Phone of string
            | Address of Address

        type ContactType = Phone | Address

        let ContactFormlet () =
            let contactTypeF =
                Controls.Select 0 [("Phone", ContactType.Phone);
                                   ("Address" , ContactType.Address)]
            Formlet.Do {
                let! contactType = contactTypeF
                return!
                    match contactType with
                    | ContactType.Address ->
                        Formlet.Map Contact.Address (AddressFormlet ())
                    | ContactType.Phone ->
                        Input ""
                        |> Validator.IsNotEmpty "Enter a valid phone number"
                        |> Enhance.WithValidationIcon
                        |> Enhance.WithTextLabel "Phone"
                        |> Formlet.Map Contact.Phone
            }

        type Order =
            {
                Name : string
                Items: List<ProductQuantity>
                Contact: Contact
            }

        let OrderFormlet () =
            let nameF =
                Input ""
                |> Validator.IsNotEmpty "Non empty name required"
                |> Enhance.WithValidationIcon
                |> Enhance.WithLegend "Name"
            let itemsF =
                (ProductFormlet () |> Enhance.WithLegend "Product")
                |> Enhance.Many
                |> Enhance.WithLegend "Items"
            let cf = ContactFormlet
            let contactF =
                ContactFormlet ()
                |> Enhance.WithLegend "Contact"

            Enhance.WithErrorSummary "Errors" (
                Formlet.Yield (fun name items contact ->
                    {
                        Name = name
                        Items = items
                        Contact = contact
                    }
                )
            )
            <*> nameF
            <*> itemsF
            <*> contactF

module Flowlet =

    [<JavaScript>]
    module Client =
        open WebSharper.Formlets
        open WebSharper.Formlets.Controls
        open FormletSample.Client

        let OrderFlowlet () =
            let fc = {
                Enhance.FormContainerConfiguration.Default with
                    Header = "Order Flowlet" |> Enhance.FormPart.Text |> Some
                }
            let nameF =
                Input ""
                |> Validator.IsNotEmpty "Non-empty name required"
                |> Enhance.WithLegend "Name"
                |> Enhance.WithSubmitAndResetButtons
                |> Enhance.WithCustomFormContainer {
                       fc with
                            Description =
                                "Step 1 - Name"
                                |> Enhance.FormPart.Text
                                |> Some
                   }
            let itemsF =
                ProductFormlet ()
                |> Enhance.WithLegend "Product"
                |> Enhance.Many
                |> Enhance.WithLegend "Items"
                |> Enhance.WithSubmitAndResetButtons
                |> Enhance.WithCustomFormContainer {
                        fc with
                            Description =
                                "Step 2 - Specify items"
                                |> Enhance.FormPart.Text
                                |> Some
                   }
            let contactTypeF =
                Controls.Select 0
                    [
                        "Phone", ContactType.Phone
                        "Address", ContactType.Address
                    ]
                |> Enhance.WithSubmitAndResetButtons
                |> Enhance.WithCustomFormContainer {
                        fc with
                            Description =
                                "Step 3 - Select contact info type"
                                |> Enhance.FormPart.Text
                                |> Some
                   }

            Formlet.Do {
                let! name = nameF
                let! items = itemsF
                let! contactType = contactTypeF
                let! contact =
                    match contactType with
                    | ContactType.Address ->
                        Formlet.Map Contact.Address (AddressFormlet ())
                        |> Enhance.WithSubmitAndResetButtons
                        |> Enhance.WithCustomFormContainer {
                            fc with
                                Description =
                                    "Step 4 - Specify address"
                                    |> Enhance.FormPart.Text
                                    |> Some
                           }
                    | ContactType.Phone ->
                        Input ""
                        |> Enhance.WithTextLabel "Phone"
                        |> Formlet.Map Contact.Phone
                        |> Enhance.WithSubmitAndResetButtons
                        |> Enhance.WithCustomFormContainer {
                            fc with
                                Description =
                                    "Step 4 - Specify phone number"
                                    |> Enhance.FormPart.Text
                                    |> Some
                           }
                return!
                    Formlet.OfElement (fun () -> Div [Text "Thanks for your order"])
                    |> Formlet.Map (fun () ->
                        {
                            Name = name
                            Items = items
                            Contact = contact
                        }
                    )
            }
            |> Formlet.Flowlet
            |> Formlet.Map ignore

type FlowletViewer() =
    inherit Web.Control()
    [<JavaScript>]
    override this.Body = Flowlet.Client.OrderFlowlet () :> _

[<EntryPoint>]
let main argv =
    //  
    0