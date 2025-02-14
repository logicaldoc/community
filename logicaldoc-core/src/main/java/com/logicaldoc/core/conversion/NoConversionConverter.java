package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;

import com.logicaldoc.util.io.FileUtil;

/**
 * A converter that simply copies the source to the destination
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.0
 */
public class NoConversionConverter extends NotAvailableConverter {

	@Override
	public void convert(File src, File dest) throws IOException {
		FileUtil.copyFile(src, dest);
	}
}