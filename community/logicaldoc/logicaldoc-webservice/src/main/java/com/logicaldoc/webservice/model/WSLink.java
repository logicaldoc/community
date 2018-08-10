package com.logicaldoc.webservice.model;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlType;

import com.logicaldoc.webservice.doc.WSDoc;

/**
 * Web Service Document Link.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.1
 */
@XmlType(name = "WSLink")
public class WSLink implements Serializable {

	@WSDoc(documented = false)
	private static final long serialVersionUID = 1L;

	@WSDoc(description = "unique identifier")
	private long id;

	@WSDoc(description = "just a string indicating the meaning of the link", required = false)
	private String type;

	@WSDoc(description = "itentifier of document 1")
	private long doc1;

	@WSDoc(description = "itentifier of document 2")
	private long doc2;

	public WSLink() {

	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public long getDoc1() {
		return doc1;
	}

	public void setDoc1(long doc1) {
		this.doc1 = doc1;
	}

	public long getDoc2() {
		return doc2;
	}

	public void setDoc2(long doc2) {
		this.doc2 = doc2;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}
}
