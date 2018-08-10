package com.logicaldoc.core.parser;

import java.io.InputStream;
import java.util.Locale;

/**
 * Parser that doesn't parse anything
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public class DummyParser extends AbstractParser {

	@Override
	public void internalParse(InputStream input, String filename, String encoding, Locale locale, String tenant, StringBuffer content) {
		
	}
}