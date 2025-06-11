package com.logicaldoc.util.servlet;

import java.io.IOException;

import jakarta.servlet.ReadListener;
import jakarta.servlet.ServletInputStream;

public class BytesServletInputStream extends ServletInputStream {

	private byte[] myBytes;

	private int lastIndexRetrieved = -1;

	private ReadListener readListener = null;

	public BytesServletInputStream(byte[] bytes) {
		this.myBytes = bytes;
	}

	public int read() throws IOException {
		int i;
		if (!isFinished()) {
			i = myBytes[lastIndexRetrieved + 1];
			lastIndexRetrieved++;
			if (isFinished() && (readListener != null)) {
				try {
					readListener.onAllDataRead();
				} catch (IOException ex) {
					readListener.onError(ex);
					throw ex;
				}
			}
			return i;
		} else {
			return -1;
		}
	}

	@Override
	public boolean isFinished() {
		return (lastIndexRetrieved == myBytes.length - 1);
	}

	@Override
	public boolean isReady() {
		return isFinished();
	}

	@Override
	public void setReadListener(ReadListener readListener) {
		this.readListener = readListener;
		if (!isFinished()) {
			try {
				readListener.onDataAvailable();
			} catch (IOException e) {
				readListener.onError(e);
			}
		} else {
			try {
				readListener.onAllDataRead();
			} catch (IOException e) {
				readListener.onError(e);
			}
		}
	}

}
