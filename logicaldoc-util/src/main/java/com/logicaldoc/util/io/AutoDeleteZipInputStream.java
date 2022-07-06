package com.logicaldoc.util.io;

import java.io.File;
import java.io.IOException;

import com.logicaldoc.util.io.FileUtil;

/**
 * Same as ZipInputStream but deletes the file after closing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.2
 */
public class AutoDeleteZipInputStream extends ZipInputStream {

	private File zipFile = null;

	public AutoDeleteZipInputStream(net.lingala.zip4j.io.ZipInputStream wrapped, File zipFile) {
		super(wrapped);
		this.zipFile = zipFile;
	}

	public void close(boolean arg0) throws IOException {
		wrapped.close(true);
		if (zipFile != null)
			FileUtil.strongDelete(zipFile);
	}
}