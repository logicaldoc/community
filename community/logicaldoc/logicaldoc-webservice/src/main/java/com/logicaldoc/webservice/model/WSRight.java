package com.logicaldoc.webservice.model;

import javax.xml.bind.annotation.XmlType;

import com.logicaldoc.webservice.doc.WSDoc;

/**
 * Useful class to associate a user or a group to a permission integer
 * representation.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
@XmlType(name = "WSRight")
public class WSRight {
	@WSDoc(description = "unique identifier of a user or a group")
	private long id;

	@WSDoc(description = "permissions mask. "
			+ "<br/> this is an integer representation of a list of 17 bits. Each bit refers to a permission: <b>0</b> to deny, <b>1</b> to grant the permission."
			+ "<br/> This list represent the bit array starting from left to right:"
			+ "<ol><li>Automation</li><li>Email</li><li>Move</li><li>Password</li><li>Print</li><li>Subscription</li><li>Calendar</li><li>Workflow</li><li>Archive</li><li>Sign</li><li>Export</li><li>Import</li>"
			+ "<li>Rename</li><li>Delete</li><li>Immutable</li><li>Security</li><li>Add</li><li>Write</li><li>Download</li><li>Read</li></ol>"
			+ "In particular, 'Read' is represented by the last right bit while 'Download' is represented by the first left bit.<br/>"
			+ "Here are two examples:"
			+ "</p><p>A) if you want to assign to a user the permissions Read, Write, Immutable, Rename, Sign, Download, the 'permissions' value must be <b>2215</b>, in fact it is <b>00000000100010100111</b> in binary representation."
			+ "</p><p>B) if you want to assign to a group the permissions Read, Write, Add, Security, Import, Archive, Workflow, the 'permissions' value must be <b>6421</b>, in fact it is <b>00000001100100010101</b> in binary representation."
			+ "</p><p><br></p>")
	private int permissions;

	public WSRight() {
	}

	public WSRight(long id, int permissions) {
		this.id = id;
		this.permissions = permissions;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public int getPermissions() {
		return permissions;
	}

	public void setPermissions(int permissions) {
		this.permissions = permissions;
	}
}
