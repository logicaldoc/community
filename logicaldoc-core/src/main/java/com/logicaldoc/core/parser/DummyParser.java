package com.logicaldoc.core.parser;

import java.io.InputStream;

/**
 * Parser that doesn't parse anything
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public class DummyParser extends AbstractParser {

	@Override
	public void internalParse(InputStream input, ParseParameters parameters, StringBuilder contentx) {
		// Noting to do
	}
}