package com.logicaldoc.core.store;

import org.apache.commons.lang.StringUtils;
import org.apache.poi.util.DocumentFormatException;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.Version;

/**
 * Represents a resource inside a {@link Store}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
public class StoreResource {

	/*
	 * The suffix for the document's resource that represents the PDF conversion
	 */
	public static final String SUFFIX_PDF_CONVERSION = "conversion.pdf";

	/*
	 * The suffix for the document's resource that represents the tile image
	 */
	public static final String SUFFIX_TILE = "tile.png";

	/*
	 * The suffix for the document's resource that represents the thumbnail
	 * image
	 */
	public static final String SUFFIX_THUMBNAIL = "thumb.png";

	/*
	 * The suffix for the document's resource that represents the mobile
	 * thumbnail image
	 */
	public static final String SUFFIX_MOBILE_THUMBNAIL = "mobile.png";

	private long docId;

	private String fileVersion;

	private String suffix;

	StoreResource(Builder builder) {
		this.fileVersion = builder.fileVersion;
		this.suffix = builder.suffix;
		this.docId = builder.docId;
	}

	public long getDocId() {
		return docId;
	}

	public String getFileVersion() {
		return fileVersion;
	}

	public String getSuffix() {
		return suffix;
	}

	/**
	 * Name of the resource as <code><b>fileVersion</b>-<b>suffix</b></code>
	 * 
	 * @return The resource's name
	 */
	public String name() {
		return sanitizeResourceName(fileVersion + (StringUtils.isEmpty(suffix) ? "" : "-" + suffix));
	}

	@Override
	public String toString() {
		return name();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (int) (docId ^ (docId >>> 32));
		result = prime * result + ((fileVersion == null) ? 0 : fileVersion.hashCode());
		result = prime * result + ((suffix == null) ? 0 : suffix.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		StoreResource other = (StoreResource) obj;
		if (docId != other.docId)
			return false;
		if (fileVersion == null) {
			if (other.fileVersion != null)
				return false;
		} else if (!fileVersion.equals(other.fileVersion))
			return false;
		if (suffix == null) {
			if (other.suffix != null)
				return false;
		} else if (!suffix.equals(other.suffix))
			return false;
		return true;
	}

	private String sanitizeResourceName(String resourceName) {
		return resourceName.replace("..", "").replaceAll("[^a-zA-Z0-9\\-\\\\.]", "");
	}

	/**
	 * Creates a new builder
	 * 
	 * @return The new builder
	 */
	public static Builder builder() {
		return new Builder();
	}

	/**
	 * A builder for {@link StoreResource}s
	 * 
	 * @author Marco Meschieri - LogicalDOC
	 * @since 9.2.2
	 */
	public static class Builder {

		private Long docId;

		private String fileVersion;

		private String suffix;

		private Builder() {
			// Empty
		}

		public Builder fileVersion(String fileVersion) {
			this.fileVersion = StringUtils.defaultString(fileVersion, this.fileVersion);
			return this;
		}

		public Builder suffix(String suffix) {
			this.suffix = StringUtils.defaultString(suffix, null);
			return this;
		}

		/**
		 * Assigns the suffix of the PDF conversion
		 * 
		 * @return The configured builder
		 */
		public Builder suffixPdfConversion() {
			return suffix(SUFFIX_PDF_CONVERSION);
		}

		/**
		 * Assigns the suffix of the tile image
		 * 
		 * @return The configured builder
		 */
		public Builder suffixTile() {
			return suffix(SUFFIX_TILE);
		}

		/**
		 * Assigns the suffix of the thumbnail image
		 * 
		 * @return The configured builder
		 */
		public Builder suffixThumbnail() {
			return suffix(SUFFIX_THUMBNAIL);
		}

		/**
		 * Assigns the suffix of the mobile thumbnail
		 * 
		 * @return The configured builder
		 */
		public Builder suffixMobile() {
			return suffix(SUFFIX_MOBILE_THUMBNAIL);
		}

		public Builder docId(long docId) {
			this.docId = docId;
			return this;
		}

		public Builder resource(StoreResource res) {
			this.docId = res.docId;
			this.fileVersion = res.fileVersion;
			this.suffix = res.suffix;
			return this;
		}

		public Builder name(String name) {
			if (name.contains("-")) {
				fileVersion = name.substring(0, name.indexOf('-')).trim();
				suffix = name.substring(name.indexOf('-') + 1, name.length()).trim();
			} else {
				fileVersion = name.trim();
			}
			return this;
		}

		/**
		 * Takes the docId and fileVersion from the given
		 * {@link DocumentFormatException} or {@link Version}
		 * 
		 * @param document The document or version
		 * 
		 * @return The configured builder
		 * 
		 * @throws PersistenceException Error in the data layer
		 */
		public Builder document(AbstractDocument document) throws PersistenceException {
			if (document instanceof Version ver)
				return version(ver);
			else if (document instanceof Document doc)
				return document(doc);
			return this;
		}

		private Builder version(Version version) {
			this.docId = version.getDocId();
			this.fileVersion = version.getFileVersion();
			if (StringUtils.isEmpty(this.fileVersion))
				this.fileVersion = version.getVersion();
			return this;
		}

		private Builder document(Document document) throws PersistenceException {
			Document realDocument = document;

			/*
			 * All versions of a document are stored in the same directory as
			 * the current version, but the filename is the version number
			 * without extension, e.g. "doc/2.1"
			 */
			if (document.getDocRef() != null) {
				// The shortcut document doesn't have the 'fileVersion' and the
				// 'version'
				realDocument = DocumentDAO.get().findById(document.getDocRef());
			}

			docId = realDocument.getId();

			if (StringUtils.isEmpty(fileVersion))
				fileVersion = realDocument.getFileVersion();
			if (StringUtils.isEmpty(fileVersion))
				this.fileVersion = realDocument.getVersion();

			return this;
		}

		public StoreResource build() {
			if (docId == null)
				throw new IllegalArgumentException("Document identifier cannot be null");
			if (StringUtils.isEmpty(fileVersion))
				throw new IllegalArgumentException("File version cannot be empty");
			return new StoreResource(this);
		}
	}
}
