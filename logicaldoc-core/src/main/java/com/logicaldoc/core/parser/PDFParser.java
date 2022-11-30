package com.logicaldoc.core.parser;

import java.io.CharArrayWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;
import java.util.List;
import java.util.Locale;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDDocumentCatalog;
import org.apache.pdfbox.pdmodel.PDDocumentInformation;
import org.apache.pdfbox.pdmodel.interactive.form.PDAcroForm;
import org.apache.pdfbox.pdmodel.interactive.form.PDField;
import org.apache.pdfbox.text.PDFTextStripper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;

/**
 * Text extractor for Portable Document Format (PDF). For parsing uses an
 * external library: PDFBox. Created on 4. November 2003, 18:09
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.5
 */
public class PDFParser extends AbstractParser {

	private static final String CAN_NOT_GET_PDF_DOCUMENT_FOR_PARSING = "Can not get pdf document for parsing";

	protected static Logger log = LoggerFactory.getLogger(PDFParser.class);

	@Override
	public void internalParse(InputStream input, String filename, String encoding, Locale locale, String tenant,
			Document document, String fileVersion, StringBuilder content) throws ParseException {
		PDDocument pdfDocument = null;
		try {
			pdfDocument = PDDocument.load(input);

			if (pdfDocument == null) {
				throw new Exception(CAN_NOT_GET_PDF_DOCUMENT_FOR_PARSING);
			} else {
				if (pdfDocument.isEncrypted()) {
					try {
						pdfDocument.close();
						pdfDocument = PDDocument.load(input, "");
					} catch (Throwable e) {
						log.error("Error: The document is encrypted.");
						content.append("The document is encrypted");
						return;
					}
				}

				if (pdfDocument == null)
					throw new Exception(CAN_NOT_GET_PDF_DOCUMENT_FOR_PARSING);

				// Strip text from the entire document
				parseDocument(pdfDocument, content);

				// Now parse the forms
				parseForm(pdfDocument, content);
			}
		} catch (Throwable ex) {
			log.error(ex.getMessage(), ex);
		} finally {
			try {
				if (pdfDocument != null)
					pdfDocument.close();
			} catch (Throwable e) {
				log.error(e.getMessage(), e);
			}
		}
	}

	/**
	 * Extract text and metadata from the main document
	 * 
	 * @throws Exception
	 */
	protected void parseDocument(PDDocument pdfDocument, StringBuilder content) throws Exception {
		PDDocumentInformation information = pdfDocument.getDocumentInformation();
		if (information == null) {
			throw new Exception("Can not get information from pdf document");
		}

		/*
		 * Incrementally read all pages
		 */
		PDFTextStripper stripper = new PDFTextStripper();
		int pages = pdfDocument.getNumberOfPages();
		for (int i = 1; i <= pages; i++) {
			try (Writer writer = new CharArrayWriter()) {
				stripper.setStartPage(i);
				stripper.setEndPage(i);
				stripper.setPageEnd("\n");
				stripper.setParagraphEnd("\n");
				stripper.setAddMoreFormatting(false);
				stripper.writeText(pdfDocument, writer);
				writer.flush();
				content.append(writer.toString());
			} catch (Throwable tw) {
				log.error("Exception reading pdf document: {}", tw.getMessage());
			}
		}
	}

	/**
	 * Extract the text from the form fields
	 */
	private void parseForm(PDDocument pdfDocument, StringBuilder content) throws IOException {
		PDDocumentCatalog docCatalog = pdfDocument.getDocumentCatalog();
		PDAcroForm acroForm = docCatalog.getAcroForm();

		if (acroForm == null)
			return;

		content.append("\n");

		List<PDField> fields = acroForm.getFields();
		log.debug("{} top-level fields were found on the form", fields.size());

		for (PDField field : fields) {
			content.append(field.getPartialName());
			content.append(" = ");
			content.append(field.getValueAsString());
			content.append(" \n ");
		}
	}

	@Override
	public int countPages(File file, String filename) {
		try {
			return internalCountPages(PDDocument.load(file));
		} catch (Throwable e) {
			if (log.isDebugEnabled())
				log.warn(e.getMessage(), e);
			return 1;
		}
	}

	@Override
	public int countPages(InputStream input, String filename) {
		try {
			return internalCountPages(PDDocument.load(input));
		} catch (Throwable e) {
			if (log.isDebugEnabled())
				log.warn(e.getMessage(), e);
			return 1;
		}
	}

	private int internalCountPages(PDDocument pdfDocument) {
		try {
			if (pdfDocument == null) {
				throw new Exception(CAN_NOT_GET_PDF_DOCUMENT_FOR_PARSING);
			} else {
				return pdfDocument.getNumberOfPages();
			}
		} catch (Throwable ex) {
			if (log.isDebugEnabled())
				log.warn(ex.getMessage(), ex);
			return 1;
		} finally {
			try {
				if (pdfDocument != null)
					pdfDocument.close();
			} catch (Throwable e) {
				if (log.isDebugEnabled())
					log.warn(e.getMessage(), e);
			}
		}
	}
}