package com.logicaldoc.core.document;

import com.logicaldoc.core.PersistentObject;

/**
 * Represents a document link. It represents a link between two documents
 * (document1 and document2). Every link can be of a certain type.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 4.0
 */
public class DocumentLink extends PersistentObject {
	
	private static final long serialVersionUID = 1L;

	private String type;

	private Document document1;

	private Document document2;

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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((document1 == null) ? 0 : document1.hashCode());
		result = prime * result + ((document2 == null) ? 0 : document2.hashCode());
		result = prime * result + ((type == null) ? 0 : type.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		DocumentLink other = (DocumentLink) obj;
		if (document1 == null) {
			if (other.document1 != null)
				return false;
		} else if (!document1.equals(other.document1))
			return false;
		if (document2 == null) {
			if (other.document2 != null)
				return false;
		} else if (!document2.equals(other.document2))
			return false;
		if (type == null) {
			if (other.type != null)
				return false;
		} else if (!type.equals(other.type))
			return false;
		return true;
	}
}