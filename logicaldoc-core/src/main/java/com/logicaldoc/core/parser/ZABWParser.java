package com.logicaldoc.core.parser;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Locale;
import java.util.zip.GZIPInputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;

/**
 * Text extractor for AbiWord compressed documents.
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.5
 */
public class ZABWParser extends AbstractParser {

	protected static Logger log = LoggerFactory.getLogger(ZABWParser.class);

	@Override
	public String parse(File file, String filename, String encoding, Locale locale, String tenant, Document document,
			String fileVersion) {
		String enc = encoding != null ? encoding : "UTF-8";
		
		try(FileInputStream stream = new FileInputStream(file);) {
			GZIPInputStream gis = new GZIPInputStream(stream);
			return parse(gis, filename, enc, locale, tenant, document, fileVersion);
		} catch (Exception ex) {
			log.warn("Failed to extract Compressed AbiWord text content", ex);
		}
		return "";
	}

	@Override
	public void internalParse(InputStream input, String filename, String encoding, Locale locale, String tenant,
			Document document, String fileVersion, StringBuffer content) {
		try {
			AbiWordParser parser = new AbiWordParser();
			content.append(parser.parse(input, filename, encoding, locale, tenant, document, fileVersion));
		} catch (Exception e) {
			log.warn("Failed to extract AbiWord Compressed zabw text content", e);
		}
	}
}