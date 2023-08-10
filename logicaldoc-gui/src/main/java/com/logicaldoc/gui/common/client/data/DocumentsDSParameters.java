package com.logicaldoc.gui.common.client.data;

/**
 * A bean to carry those parameters for a document datasource
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.5
 */
public class DocumentsDSParameters {
	/**
	 * The folder to be listed (optional)
	 */
	private Long folderId;

	/**
	 * A filter on the file nale (optional)
	 */
	private String fileFilter;

	/**
	 * The maximum number of records (if not specified MAX_ROWS is used)
	 */
	private Integer max;

	private int page;

	/**
	 * The indexed flag
	 */
	private Integer indexed;

	/**
	 * The barcoded flag
	 */
	private boolean barcoded = false;

	/**
	 * The ocrd flag
	 */
	private boolean ocrd = false;

	/**
	 * The sort specification (optional)
	 */
	private String sortSpec;

	public DocumentsDSParameters(Long folderId, String fileFilter, Integer max, int page, String sortSpec) {
		this.folderId = folderId;
		this.fileFilter = fileFilter;
		this.max = max;
		this.page = page;
		this.sortSpec = sortSpec;
	}

	public Long getFolderId() {
		return folderId;
	}

	public void setFolderId(Long folderId) {
		this.folderId = folderId;
	}

	public String getFileFilter() {
		return fileFilter;
	}

	public void setFileFilter(String fileFilter) {
		this.fileFilter = fileFilter;
	}

	public Integer getMax() {
		return max;
	}

	public void setMax(Integer max) {
		this.max = max;
	}

	public int getPage() {
		return page;
	}

	public void setPage(int page) {
		this.page = page;
	}

	public Integer getIndexed() {
		return indexed;
	}

	public void setIndexed(Integer indexed) {
		this.indexed = indexed;
	}

	public boolean isBarcoded() {
		return barcoded;
	}

	public void setBarcoded(boolean barcoded) {
		this.barcoded = barcoded;
	}

	public boolean isOcrd() {
		return ocrd;
	}

	public void setOcrd(boolean ocrd) {
		this.ocrd = ocrd;
	}

	public String getSortSpec() {
		return sortSpec;
	}

	public void setSortSpec(String sortSpec) {
		this.sortSpec = sortSpec;
	}
}