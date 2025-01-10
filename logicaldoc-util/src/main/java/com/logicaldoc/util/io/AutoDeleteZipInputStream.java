package com.logicaldoc.util.io;

import java.io.File;
import java.io.IOException;

/**
 * Same as ZipInputStream but deletes the file after closing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.2
 */
public class AutoDeleteZipInputStream extends ZipInputStream {

	private File zipFile = null;

	public AutoDeleteZipInputStream(net.lingala.zip4j.io.inputstream.ZipInputStream wrapped, File zipFile) {
		super(wrapped);
		this.zipFile = zipFile;
	}

	@Override
	public void close() throws IOException {
		wrapped.close();
		FileUtil.delete(zipFile);
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}