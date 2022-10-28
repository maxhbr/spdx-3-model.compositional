use iri_string::types::IriStr;
use serde_derive::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize)] 
struct CreationInformation {

}

trait Element {
    // fn spdxid(&self) -> IriStr;
    fn name(&self) -> String;
}

#[derive(Debug, Deserialize, Serialize)] 
struct ElementImpl {
    spdxid: Box<IriStr>,
    name: String,
    summary: String,
    description: String,
    comment: String,
}

// impl Element for ElementImpl {
//     // fn spdxid(&self) -> IriStr {
//     //     *self.spdxid()
//     // }
//     fn name(&self) -> String {
//         *self.name
//     }
// }


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let e = ElementImpl {
            spdxid: IriStr::new("urn:spdx.dev").unwrap().into(),
            name: String::from("Element1"),
            summary: String::from("Summary1"),
            description: String::from("Description1"),
            comment: String::from("Comment1")
        };
        assert_eq!(e.name, "Element1".to_string());
    }
}
