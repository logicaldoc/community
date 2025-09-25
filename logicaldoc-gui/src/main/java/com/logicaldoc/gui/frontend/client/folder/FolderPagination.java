package com.logicaldoc.gui.frontend.client.folder;

/**
 * Saves the pagination settings of a folder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.2
 */
public class FolderPagination {

	private int pageSize = 1000;

	private int totalElements = 0;

	private int page = 1;

	private long folderId = 0L;

	public FolderPagination(long folderId, int pageSize, int totalElements, int page) {
		super();
		this.folderId = folderId;
		this.pageSize = pageSize;
		this.totalElements = totalElements;
		this.page = page;
	}

	public int getPageSize() {
		return pageSize;
	}

	public int getPage() {
		return page;
	}

	public void setPageSize(int pageSize) {
		this.pageSize = pageSize;
	}

	public void setPage(int page) {
		this.page = page;
	}

	public int getTotalElements() {
		return totalElements;
	}

	public void setTotalElements(int totalElements) {
		this.totalElements = totalElements;
	}

	public int getTotalPages() {
		return (totalElements + pageSize - 1) / pageSize;
	}

	public int getStartRow() {
		return (page - 1) * pageSize;
	}

	public long getFolderId() {
		return folderId;
	}

	public void setFolderId(long folderId) {
		this.folderId = folderId;
	}
}