package com.logicaldoc.core.parser;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;

import org.apache.poi.hssf.extractor.ExcelExtractor;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.StringUtil;

/**
 * Parser for Office 2003 worksheets
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.5
 */
public class XLSParser extends AbstractParser {

	private static final Logger log = LoggerFactory.getLogger(XLSParser.class);

	@Override
	public void internalParse(InputStream input, ParseParameters parameters, StringBuilder content) {

		try (POIFSFileSystem fs = new POIFSFileSystem(input); ExcelExtractor extractor = new ExcelExtractor(fs);) {
			String tmp = extractor.getText();

			// Replace Control characters
			if (tmp != null)
				tmp = tmp.replaceAll("[\\p{Cntrl}&&[^\\n]]", " ");

			content.append(StringUtil.writeToString(new StringReader(tmp)));
		} catch (Exception e) {
			log.warn("Failed to extract Excel text content", e);
		}
	}

	@Override
	public int countPages(InputStream input, String filename) {
		try (HSSFWorkbook excelDoc = new HSSFWorkbook(input)) {
			return excelDoc.getNumberOfSheets();
		} catch (IOException e) {
			log.error(e.getMessage(), e);
		}
		return 1;
	}
}