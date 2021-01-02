## Meet the LEGO Art Mosaic Generator

### What is this?

This is a fun Holiday project that started with a Christmas present. 
The relatively new [LEGO Art theme sets](https://www.lego.com/en-us/themes/art/about) allow the construction of neat 48*48 mosaics. They come with parts sets of differently colored mosaic tiles. Obviously it is very tempting to create your own mosaics with the sets. The challenge is, however, that each set comes with a limited tile set and a unique color palette. This makes conversion of an image non-trivial.

There already is at least [one very nice online tool](https://lego-art-remix.debkbanerji.com) by Dab Banerji that allows you to convert a given bitmap image file to a mosaic obeying these restrictions. Playing around with it, I noticed that the limitations of the Art sets make it very hard (if not almost impossible) to create a satisfying mosaic automatically. In the end, an automated 'first shot' along with some manual editing tools seemed like a useful alternative approach. Here we go. 


### Where do we stand?

While this is still work in progress, the basic functionality is there. What works

- [x] You can select from the various LEGO Art sets
- [x] You an upload your own square bitmap image file
- [x] It will be converted to 48x48 pixels
- [x] A linear programming approach can be used to convert it to a mosaic, complying to tile restrictions imposed by the chosen LEGO Arts set
- [x] Images can be modified pre and post conversion to get better looking mosaics
- [x] For that, pixels can be selected individually, by color, by area (flood fill) or by whether they are not yet colored in a mosaic color
- [x] Selected pixels can be automatically or manually colored
- [x] Multilevel undo/redo (thanks to the brilliant module of the [{shinyThings} package](https://github.com/gadenbuie/shinyThings))
- [x] The resulting image can be stored as a TIFF file
- [x] Building instructions are provided as a PDF file


### What is still missing

- [ ] An option for additional selections (using 'Shift' + Click)
- [ ] A blog post to tell a story on rapid prototyping of fun but useless things over Christmas using the power of Open Source development 


### Can I just... use it?

Yes: [Clic here](https://jgassen.shinyapps.io/legoartmosaic/)! No worries: Your image will only processed in server-side memory and not stored. Be advised: The app is not done but it works (TM). Use at your own risk. I would encourage you to save your work when you start modifying your mosaic by downloading the resulting image from time to time. When you load a partly converted saved image, make sure to adjust the number of colors so that the app does not compress the mosaic colors. 


### Disclaimer

I am not affiliated with LEGO in any kind and this project is not meant to infringe any rights. The data to build the LEGO Art color palettes and part lists has been obtained from https://rebrickable.com.
