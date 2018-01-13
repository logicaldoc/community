package com.logicaldoc.core.document;

import com.logicaldoc.core.PersistentObject;

/**
 * Represents a document link. It represents a link between two documents
 * (document1 and document2). Every link can be of a certain type.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 4.0
 */
public class DocumentLink extends PersistentObject {

	private String type;

	private Document document1;

	private Document document2;

	public DocumentLink() {

	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public Document getDocument1() {
		return document1;
	}

	public void setDocument1(Document document1) {
		this.document1 = document1;
	}

	public Document getDocument2() {
		return document2;
	}

	public void setDocument2(Document document2) {
		this.document2 = document2;
	}
}
