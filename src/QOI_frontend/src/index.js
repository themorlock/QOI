import { QOI_backend } from "../../declarations/QOI_backend";

document.querySelector("form").addEventListener("submit", async (e) => {
  e.preventDefault();
  console.log("Uploading file");

  let fileInput = document.getElementById("photo");
  fileInput.onchange = e => {
    var file = e.target.files[0]; 

    // get a reader from the selected file and read bytes
    var reader = new FileReader();
    reader.readAsArrayBuffer(file);

    reader.onload = readerEvent => {
        var content = readerEvent.target.result; // this is the content!

        // interpret read buffer as a byte array
        let view = new Uint8Array(content);
        // remove padding from beginning of ppm format
        view = view.slice(15)
        // encode using sample values of width, height, channels, colorspace
        QOI_backend.encodeBitmap({desc: {width: view.length / 2 / 3, height: 2, channels: 3, colorspace: 1}, data: view}).then((encodeResult) => {
          console.log("Encoding results:");
          console.log(encodeResult);
          console.log("\n");
          QOI_backend.decodeQOI(encodeResult.ok).then((decodeResult) => {
          console.log("Decoding results:");
            console.log(decodeResult);
          });
        });

        console.log("Initial content:")
        console.log(content);
        console.log("\n")
    }
  }
  fileInput.click();
});
