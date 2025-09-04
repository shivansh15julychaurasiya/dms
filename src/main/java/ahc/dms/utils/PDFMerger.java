package ahc.dms.utils;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import com.itextpdf.text.Document;
import com.itextpdf.text.DocumentException;
import com.itextpdf.text.Rectangle;
import com.itextpdf.text.pdf.BaseFont;
import com.itextpdf.text.pdf.PdfContentByte;
import com.itextpdf.text.pdf.PdfImportedPage;
import com.itextpdf.text.pdf.PdfReader;
import com.itextpdf.text.pdf.PdfWriter;
import com.itextpdf.text.pdf.SimpleBookmark;

public class PDFMerger{
	
//  ********************** Fullstack Java Developer Vijay Chaurasiya *******************************

	 public static void doMerge(List<InputStream> streamOfPDFFiles, OutputStream outputStream, boolean paginate) throws DocumentException {
		  Rectangle size=new Rectangle(612, 1040);
		 // PdfStamper stamper=null;
		    Document document = new Document(size);
		    List<HashMap<String, Object>> bookmarks = new ArrayList<HashMap<String, Object>>();
			List<HashMap<String, Object>> tempBookmarks;

			int pag = 0;
			
			int j = 0;

			int page_offset = 0;
		    try {
		 
		      List<InputStream> pdfs = streamOfPDFFiles;
		      List<PdfReader> readers = new ArrayList<PdfReader>();
		      
		      int totalPages = 0;
		      Iterator<InputStream> iteratorPDFs = pdfs.iterator();

		      // Create Readers for the pdfs.
		      while (iteratorPDFs.hasNext()) {
		        InputStream pdf = iteratorPDFs.next();
		        PdfReader pdfReader = new PdfReader(pdf);
		        readers.add(pdfReader);
		        totalPages += pdfReader.getNumberOfPages();
		      }
		      // Create a writer for the outputstream
		      PdfWriter writer = PdfWriter.getInstance(document, outputStream);

		      document.open();
		      BaseFont bf = BaseFont.createFont(BaseFont.HELVETICA, BaseFont.CP1252, BaseFont.NOT_EMBEDDED);
		      PdfContentByte cb = writer.getDirectContent(); // Holds the PDF
		      // data

		      PdfImportedPage page;
		     
		      int currentPageNumber = 0;
		      int pageOfCurrentReaderPDF = 0;
		      Iterator<PdfReader> iteratorPDFReader = readers.iterator();

		      // Loop through the PDF files and add to the output.
		      while (iteratorPDFReader.hasNext()) {
		          PdfReader pdfReader = iteratorPDFReader.next();
		         // stamper = new PdfStamper(pdfReader, outputStream);
		          
		          pag = pdfReader.getNumberOfPages();

					tempBookmarks = SimpleBookmark.getBookmark(pdfReader);

					if (j == 0 && tempBookmarks != null) {

						SimpleBookmark.shiftPageNumbers(tempBookmarks, page_offset, null);

						page_offset += pag;
						if (tempBookmarks != null)
							bookmarks.addAll(tempBookmarks);
		// MessageBox.Show(n.ToString());
						totalPages = pag;
					}

					else {
						SimpleBookmark.shiftPageNumbers(tempBookmarks, page_offset, null);
						if (tempBookmarks != null)
							bookmarks.addAll(tempBookmarks);

						page_offset += pag;
						totalPages += pag;
					}

		          // Create a new page in the target for each source page.
		          while (pageOfCurrentReaderPDF < pdfReader.getNumberOfPages()) {
		        	  
		            document.newPage();
		            pageOfCurrentReaderPDF++;
		            currentPageNumber++;
		            page = writer.getImportedPage(pdfReader, pageOfCurrentReaderPDF);
		           /* Iterator<HashMap<String, Object>> bookmark1=bookmarks.iterator();
		            while (bookmark1.hasNext()) {
		            	HashMap<String, Object> bookmark =bookmark1.next();
		            	
		            	if(bookmark.containsValue(pageOfCurrentReaderPDF)) {
		            		 writer.setOutlines(bookmark);
		            	}
						
					}*/
		           
		            cb.addTemplate(page, 0, 0);

		            // Code for pagination.
		            if (paginate) {
		              cb.beginText();
		              cb.setFontAndSize(bf, 9);
		              cb.showTextAligned(PdfContentByte.ALIGN_CENTER, "" + currentPageNumber + " of " + totalPages, 520, 5, 0);
		              cb.endText();
		            }
		          }
		          writer.setOutlines(bookmarks);
		          //stamper.setOutlines(bookmarks);
		          pageOfCurrentReaderPDF = 0;
		        }
		      document.close();
		      //stamper.close();
		      outputStream.flush();
		      
		      
		      outputStream.close();
		    } catch (Exception e) {
		      e.printStackTrace();
		    } finally {
		      if (document.isOpen())
		        document.close();
		      try {
		        if (outputStream != null)
		        	
		          outputStream.close();
		      } catch (IOException ioe) {
		        ioe.printStackTrace();
		      }
		    }
		  }
}