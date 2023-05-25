package com.logicaldoc.util.http;

import java.io.File;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import org.apache.http.entity.ContentType;
import org.apache.http.entity.mime.content.FileBody;

/**
 * This is a file body that notifies about the progess a given listener
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class FileBodyCounter extends FileBody {
	private long byteCount;

	private ProgressListener listener;

	public FileBodyCounter(File file, ContentType contentType, String filename, ProgressListener listener) {
		super(file, contentType, filename);
		this.listener = listener;
	}

	public FileBodyCounter(File file, ProgressListener listener) {
		super(file);
		this.listener = listener;
	}

	public long getBytesWritten() {
		return byteCount;
	}

	@Override
	public void writeTo(OutputStream out) throws IOException {
		super.writeTo(new FilterOutputStream(out) {
			// Other write() methods omitted for brevity.
			// Implement for better performance
			@Override
			public void write(int b) throws IOException {
				byteCount++;
				super.write(b);
				listener.transferred(byteCount, 1);
			}
		});
	}

	public static interface ProgressListener {
		void transferred(long total, long increment);
	}
}
