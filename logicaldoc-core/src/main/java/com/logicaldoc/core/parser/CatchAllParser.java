package com.logicaldoc.core.parser;

import java.io.File;
import java.io.InputStream;

import org.apache.commons.io.FileUtils;

import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.spring.Context;

/**
 * Parser that tries to convert the document into PDF and then tries to parse it
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class CatchAllParser extends AbstractParser {

	@Override
	public void internalParse(InputStream input, ParseParameters parameters, StringBuilder content) {
		if (parameters.getDocument() == null)
			parse1(input, parameters, content);
		else
			parse2(parameters, content);
	}

	/**
	 * Parses with document specification
	 */
	private void parse2(ParseParameters parameters, StringBuilder content) {
		try {
			FormatConverterManager manager = Context.get(FormatConverterManager.class);
			manager.convertToPdf(parameters.getDocument(), parameters.getFileVersion(), null);

			Store store = Context.get(Store.class);
			String pdfResource = store
					.getResourceName(parameters.getDocument(),
							parameters.getFileVersion() != null ? parameters.getFileVersion()
									: parameters.getDocument().getFileVersion(),
							FormatConverterManager.PDF_CONVERSION_SUFFIX);
			if (store.exists(parameters.getDocument().getId(), pdfResource)) {
				Parser parser = ParserFactory.getParser("pdf");
				content.append(parser.parse(store.getStream(parameters.getDocument().getId(), pdfResource),
						new ParseParameters(null, "output.pdf", null, parameters.getEncoding(), parameters.getLocale(),
								parameters.getTenant())));
			} else
				log.warn("Cannot convert the file {} into pdf in order to parse the contents",
						parameters.getFileName());
		} catch (Exception t) {
			log.error(t.getMessage(), t);
		}
	}

	/**
	 * Parses without document specification
	 */
	private void parse1(InputStream input, ParseParameters parameters, StringBuilder content) {
		FormatConverterManager manager = Context.get(FormatConverterManager.class);
		File inputFile = null;
		File outputPdf = null;
		try {
			inputFile = FileUtil.createTempFile("input", parameters.getFileName());
			outputPdf = FileUtil.createTempFile("output", ".pdf");

			// Copy the input stream into a temporary file and convert into PDF
			FileUtils.copyInputStreamToFile(input, inputFile);
			if (inputFile.exists() && inputFile.length() > 0) {
				manager.convertFile(inputFile, parameters.getFileName(), outputPdf, "pdf", null);
				if (outputPdf.exists() && outputPdf.length() > 0) {
					Parser parser = ParserFactory.getParser("pdf");
					content.append(parser.parse(outputPdf, "output.pdf", parameters.getEncoding(),
							parameters.getLocale(), parameters.getTenant(), null, null));
				} else {
					log.warn("Cannot convert the file {} into pdf in order to parse the contents",
							parameters.getFileName());
				}
			}
		} catch (Exception t) {
			log.error(t.getMessage(), t);
		} finally {
			if (inputFile != null && inputFile.exists())
				FileUtil.delete(inputFile);
			if (outputPdf != null && outputPdf.exists())
				FileUtil.delete(outputPdf);
		}
	}
}