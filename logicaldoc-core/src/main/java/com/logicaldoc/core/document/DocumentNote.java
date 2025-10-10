package com.logicaldoc.core.document;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.hibernate.LazyInitializationException;

import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.Secure;

import jakarta.persistence.Cacheable;
import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.Table;

/**
 * A note over a document
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
@Entity
@Table(name = "ld_note")
@Cacheable
public class DocumentNote extends PersistentObject implements Secure<NoteAccessControlEntry> {

	private static final long serialVersionUID = 1L;

	public static final String SHAPE_SQUARE = "square";

	public static final String SHAPE_CIRCLE = "circle";

	public static final String SHAPE_LINE = "line";

	public static final String SHAPE_ARROW = "arrow";

	public static final String SHAPE_THICKARROW = "thickarrow";

	public static final String SHAPE_COMMENT = "comment";

	public static final String SHAPE_LABEL = "label";

	@Column(name = "ld_docid", nullable = false)
	private long docId;

	@Column(name = "ld_fileversion", length = 10)
	private String fileVersion;

	@Column(name = "ld_filename", length = 255)
	private String fileName;

	/**
	 * Unique username of the user that created this note
	 */
	@Column(name = "ld_username", length = 255)
	private String username;

	/**
	 * Unique identifier of the user that created this note
	 */
	@Column(name = "ld_userid")
	private long userId;

	@Column(name = "ld_date", columnDefinition = "DATETIME(3)")
	private Date date = new Date();

	@Column(name = "ld_message", length = 4000)
	private String message;

	@Column(name = "ld_page", nullable = false)
	private int page = 0;

	@Column(name = "ld_opacity", nullable = false)
	private int opacity = 80;

	@Column(name = "ld_color", length = 255)
	private String color = "#FFCC00";

	@Column(name = "ld_left", nullable = false)
	private double left = 0.5;

	@Column(name = "ld_top", nullable = false)
	private double top = 0.5;

	@Column(name = "ld_width", nullable = false)
	private double width = 0.15;

	@Column(name = "ld_height", nullable = false)
	private double height = 0.10;

	/**
	 * A type of note, for normal notes it is null
	 */
	@Column(name = "ld_type", length = 255)
	private String type;

	/**
	 * A reference to a recipient, it could be a username or the full name of a
	 * person, normally this field is not used
	 */
	@Column(name = "ld_recipient", length = 255)
	private String recipient;

	/**
	 * An email associated to the note, normally this field is not used
	 */
	@Column(name = "ld_recipientemail", length = 255)
	private String recipientEmail;

	@Column(name = "ld_shape")
	private String shape = SHAPE_SQUARE;

	@Column(name = "ld_linewidth", nullable = false)
	private int lineWidth = 1;

	@Column(name = "ld_lineopacity", nullable = false)
	private int lineOpacity = 80;

	@Column(name = "ld_linecolor", length = 255)
	private String lineColor = "#a1a1a1";

	@Column(name = "ld_rotation", nullable = false)
	private double rotation = 0.0;

	@ElementCollection
	@CollectionTable(name = "ld_note_acl", joinColumns = @JoinColumn(name = "ld_noteid"))
	private Set<NoteAccessControlEntry> accessControlList = new HashSet<>();

	public DocumentNote() {
	}

	public DocumentNote(DocumentNote source) {
		this.docId = source.docId;
		this.fileVersion = source.fileVersion;
		this.fileName = source.fileName;
		this.userId = source.userId;
		this.username = source.username;
		this.date = source.date;
		this.message = source.message;
		this.page = source.page;
		this.opacity = source.opacity;
		this.color = source.color;
		this.left = source.left;
		this.top = source.top;
		this.width = source.width;
		this.height = source.height;
		this.lineOpacity = source.lineOpacity;
		this.lineColor = source.lineColor;
		this.lineWidth = source.lineWidth;
		this.shape = source.shape;
		this.rotation = source.rotation;
		this.type = source.type;
		this.recipient = source.recipient;
		this.recipientEmail = source.recipientEmail;
		setTenantId(source.getTenantId());

		try {
			for (NoteAccessControlEntry ace : source.getAccessControlList())
				getAccessControlList().add(new NoteAccessControlEntry(ace));
		} catch (LazyInitializationException x) {
			// may happen do nothing
		}
	}

	public long getDocId() {
		return docId;
	}

	public void setDocId(long docId) {
		this.docId = docId;
	}

	public long getUserId() {
		return userId;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public Date getDate() {
		return date;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public int getPage() {
		return page;
	}

	public void setPage(int page) {
		this.page = page;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public int getOpacity() {
		return opacity;
	}

	public String getColor() {
		return color;
	}

	public double getLeft() {
		return left;
	}

	public double getTop() {
		return top;
	}

	public double getWidth() {
		return width;
	}

	public double getHeight() {
		return height;
	}

	public void setOpacity(int opacity) {
		this.opacity = opacity;
	}

	public void setColor(String color) {
		this.color = color;
	}

	public void setLeft(double left) {
		this.left = left;
	}

	public void setTop(double top) {
		this.top = top;
	}

	public void setWidth(double width) {
		this.width = width;
	}

	public void setHeight(double height) {
		this.height = height;
	}

	public String getFileVersion() {
		return fileVersion;
	}

	public void setFileVersion(String fileVersion) {
		this.fileVersion = fileVersion;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getRecipient() {
		return recipient;
	}

	public void setRecipient(String recipient) {
		this.recipient = recipient;
	}

	public String getRecipientEmail() {
		return recipientEmail;
	}

	public void setRecipientEmail(String recipientEmail) {
		this.recipientEmail = recipientEmail;
	}

	public int getLineOpacity() {
		return lineOpacity;
	}

	public void setLineOpacity(int lineOpacity) {
		this.lineOpacity = lineOpacity;
	}

	public String getLineColor() {
		return lineColor;
	}

	public void setLineColor(String lineColor) {
		this.lineColor = lineColor;
	}

	public int getLineWidth() {
		return lineWidth;
	}

	public void setLineWidth(int lineWidth) {
		this.lineWidth = lineWidth;
	}

	public String getShape() {
		return shape;
	}

	public void setShape(String shape) {
		this.shape = shape;
	}

	public double getRotation() {
		return rotation;
	}

	public void setRotation(double rotation) {
		this.rotation = rotation;
	}

	@Override
	public void setAccessControlList(Set<NoteAccessControlEntry> acl) {
		accessControlList = acl;
	}

	@Override
	public Set<NoteAccessControlEntry> getAccessControlList() {
		return accessControlList;
	}

	@Override
	public AccessControlEntry getAccessControlEntry(long groupId) {
		return getAccessControlList().stream().filter(ace -> ace.getGroupId() == groupId).findFirst().orElse(null);
	}

	@Override
	public Set<NoteAccessControlEntry> getAccessControlEntries(Set<Long> groupIds) {
		return getAccessControlList().stream().filter(ace -> groupIds.contains(ace.getGroupId()))
				.collect(Collectors.toSet());
	}

	@Override
	public void addAccessControlEntry(NoteAccessControlEntry ace) {
		if (!getAccessControlList().add(ace)) {
			getAccessControlList().remove(ace);
			getAccessControlList().add(ace);
		}
	}

	@Override
	public String toString() {
		return StringUtils.abbreviate(message, 40) + "(" + getId() + ")";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((date == null) ? 0 : date.hashCode());
		result = prime * result + (int) (docId ^ (docId >>> 32));
		result = prime * result + ((fileVersion == null) ? 0 : fileVersion.hashCode());
		result = prime * result + page;
		result = prime * result + (int) (userId ^ (userId >>> 32));
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
		DocumentNote other = (DocumentNote) obj;
		if (date == null) {
			if (other.date != null)
				return false;
		} else if (!date.equals(other.date))
			return false;
		if (docId != other.docId)
			return false;
		if (fileVersion == null) {
			if (other.fileVersion != null)
				return false;
		} else if (!fileVersion.equals(other.fileVersion))
			return false;
		if (page != other.page)
			return false;
		return userId == other.userId;
	}
}