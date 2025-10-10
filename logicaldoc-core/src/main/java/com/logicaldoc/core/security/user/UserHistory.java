package com.logicaldoc.core.security.user;

import com.logicaldoc.core.history.History;
import com.logicaldoc.core.security.Session;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

/**
 * History entry due to an event on a user.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.0
 */
@Entity
@Table(name = "ld_user_history")
@Cacheable
public class UserHistory extends History {

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_author", length = 255)
	private String author;

	@Column(name = "ld_filename", length = 255)
	private String filename = null;

	@Column(name = "ld_filesize")
	private Long fileSize = null;

	@Column(name = "ld_docid")
	private Long docId;

	@Column(name = "ld_folderid")
	private Long folderId;

	public UserHistory() {
		super();
	}

	public UserHistory(Session session) {
		super();
		setSession(session);
	}

	public UserHistory(UserHistory source) {
		copyAttributesFrom(source);
		this.author = source.author;
		this.filename = source.filename;
		this.fileSize = source.fileSize;
		this.folderId = source.folderId;
		this.docId = source.docId;
	}

	public void setEvent(UserEvent event) {
		this.event = (event != null) ? event.toString() : null;
	}

	@Override
	public void setEvent(String event) {
		setEvent(UserEvent.fromKey(event));
	}

	public UserEvent getEventEnum() {
		if (event == null)
			return null;
		return UserEvent.fromKey(event);
	}

	public Long getFolderId() {
		return folderId;
	}

	public void setFolderId(Long folderId) {
		this.folderId = folderId;
	}

	public String getAuthor() {
		return author;
	}

	public void setAuthor(String author) {
		this.author = author;
	}

	public String getFilename() {
		return filename;
	}

	public void setFilename(String filename) {
		this.filename = filename;
	}

	public Long getFileSize() {
		return fileSize;
	}

	public void setFileSize(Long fileSize) {
		this.fileSize = fileSize;
	}

	public Long getDocId() {
		return docId;
	}

	public void setDocId(Long docId) {
		this.docId = docId;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((author == null) ? 0 : author.hashCode());
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
		UserHistory other = (UserHistory) obj;
		if (author == null) {
			if (other.author != null)
				return false;
		} else if (!author.equals(other.author))
			return false;
		return true;
	}
}