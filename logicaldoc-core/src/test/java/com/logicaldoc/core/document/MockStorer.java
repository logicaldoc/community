package com.logicaldoc.core.document;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import com.logicaldoc.core.store.FSStorer;

/**
 * This is basically a {@link FSStorer} but with a flag that if active makes the
 * store method to return an exception
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class MockStorer extends FSStorer {
	private boolean raiseError = false;

	private boolean useDummyFile = false;

	public boolean isRaiseError() {
		return raiseError;
	}

	public void setRaiseError(boolean raiseError) {
		this.raiseError = raiseError;
	}

	@Override
	public void store(File file, long docId, String resource) throws IOException {
		if (raiseError)
			throw new IOException("error");
		if (useDummyFile)
			super.store(new File("pom.xml"), docId, resource);
		else
			super.store(file, docId, resource);
	}

	@Override
	public void store(InputStream stream, long docId, String resource) throws IOException {
		if (raiseError)
			throw new IOException("error");
		if (useDummyFile)
			super.store(new FileInputStream("pom.xml"), docId, resource);
		else
			super.store(stream, docId, resource);
	}

	@Override
	public InputStream getStream(long docId, String resource) throws IOException {
		if (useDummyFile)
			return new FileInputStream("pom.xml");
		else
			return super.getStream(docId, resource);
	}

	public boolean isUseDummyFile() {
		return useDummyFile;
	}

	public void setUseDummyFile(boolean useDummyFile) {
		this.useDummyFile = useDummyFile;
	}
}