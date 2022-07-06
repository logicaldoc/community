package com.logicaldoc.core.parser;

import java.io.File;
import java.io.InputStream;
import java.util.Locale;

import org.apache.commons.io.FileUtils;

import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;

/**
 * Parser that tries to convert the document into PDF and then tries to parse it
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class CatchAllParser extends AbstractParser {

	@Override
	public void internalParse(InputStream input, String filename, String encoding, Locale locale, String tenant,
			Document document, String fileVersion, StringBuffer content) {
		if (document == null)
			parse1(input, filename, encoding, locale, tenant, content);
		else
			parse2(input, filename, encoding, locale, tenant, document, fileVersion, content);
	}

	/**
	 * Parses with document specification
	 */
	private void parse2(InputStream input, String filename, String encoding, Locale locale, String tenant,
			Document document, String fileVersion, StringBuffer content) {

		try {
			FormatConverterManager manager = (FormatConverterManager) Context.get()
					.getBean(FormatConverterManager.class);
			manager.convertToPdf(document, fileVersion, null);

			Storer storer = (Storer) Context.get().getBean(Storer.class);
			String pdfResource = storer.getResourceName(document,
					fileVersion != null ? fileVersion : document.getFileVersion(),
					FormatConverterManager.PDF_CONVERSION_SUFFIX);
			if (storer.exists(document.getId(), pdfResource)) {
				Parser parser = ParserFactory.getParser("pdf");
				content.append(parser.parse(storer.getStream(document.getId(), pdfResource), "output.pdf", encoding,
						locale, tenant, null, null));
			} else
				log.warn("Cannot convert the file {} into pdf in order to parse the contents", filename);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		}
	}

	/**
	 * Parses without document specification
	 */
	private void parse1(InputStream input, String filename, String encoding, Locale locale, String tenant,
			StringBuffer content) {
		FormatConverterManager manager = (FormatConverterManager) Context.get().getBean(FormatConverterManager.class);
		File inputFile = null;
		File outputPdf = null;
		try {
			inputFile = File.createTempFile("input", filename);
			outputPdf = File.createTempFile("output", ".pdf");

			// Copy the input stream into a temporary file and convert into PDF
			FileUtils.copyInputStreamToFile(input, inputFile);
			if (inputFile.exists() && inputFile.length() > 0) {
				manager.convertFile(inputFile, filename, outputPdf, "pdf", null);
				if (outputPdf.exists() && outputPdf.length() > 0) {
					Parser parser = ParserFactory.getParser("pdf");
					content.append(parser.parse(outputPdf, "output.pdf", encoding, locale, tenant, null, null));
				} else {
					log.warn("Cannot convert the file {} into pdf in order to parse the contents", filename);
				}
			}
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		} finally {
			if (inputFile != null && inputFile.exists())
				FileUtil.strongDelete(inputFile);
			if (outputPdf != null && outputPdf.exists())
				FileUtil.strongDelete(outputPdf);
		}
	}
}