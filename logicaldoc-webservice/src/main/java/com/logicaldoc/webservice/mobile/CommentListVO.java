package com.logicaldoc.webservice.mobile;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import jakarta.xml.bind.annotation.XmlRootElement;

/**
 * User: Niraj Singh Date: 06/05/14
 */
@XmlRootElement(name = "CommentList")
public class CommentListVO implements Serializable {

	private static final long serialVersionUID = 1L;

	private int total;

	private int pageSize;

	private int startIndex;

	private int itemCount;

	private PermissionVO nodePermissions = new PermissionVO();

	private List<CommentVO> items = new ArrayList<>();

	public void setTotal(int total) {
		this.total = total;
	}

	public void setPageSize(int pageSize) {
		this.pageSize = pageSize;
	}

	public void setStartIndex(int startIndex) {
		this.startIndex = startIndex;
	}

	public void setItemCount(int itemCount) {
		this.itemCount = itemCount;
	}

	public int getTotal() {
		return total;
	}

	public int getPageSize() {
		return pageSize;
	}

	public int getStartIndex() {
		return startIndex;
	}

	public int getItemCount() {
		return itemCount;
	}

	public List<CommentVO> getItems() {
		return items;
	}

	public void setItems(List<CommentVO> commentList) {
		this.items = commentList;
	}

	public PermissionVO getNodePermissions() {
		return nodePermissions;
	}

}