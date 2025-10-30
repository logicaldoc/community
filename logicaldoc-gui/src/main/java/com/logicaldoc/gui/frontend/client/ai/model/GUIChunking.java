package com.logicaldoc.gui.frontend.client.ai.model;

import java.io.Serializable;

/**
 * Carries the settings used to extract multiple chunks from a bigger document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
public class GUIChunking implements Serializable {

	private static final long serialVersionUID = 1L;

	/**
	 * Target number of tokens per chunk
	 */
	private int chunkSize = 800;

	/**
	 * Minimum number of tokens per chunk
	 */
	private int minChunkSize = 5;

	/**
	 * Avoid tiny tails
	 */
	private int minChunkSizeChars = 350;

	/**
	 * Maximum number of chunks per document
	 */
	private int maxChunks = 10_000;

	public int getMinChunkSizeChars() {
		return minChunkSizeChars;
	}

	public void setMinChunkSizeChars(int minChunkSizeChars) {
		this.minChunkSizeChars = minChunkSizeChars;
	}

	public int getMaxChunks() {
		return maxChunks;
	}

	public void setMaxChunks(int maxChunks) {
		this.maxChunks = maxChunks;
	}

	public int getMinChunkSize() {
		return minChunkSize;
	}

	public void setMinChunkSize(int minChunkSize) {
		this.minChunkSize = minChunkSize;
	}

	public int getChunkSize() {
		return chunkSize;
	}

	public void setChunkSize(int chunkSize) {
		this.chunkSize = chunkSize;
	}
}