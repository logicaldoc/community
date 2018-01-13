package com.logicaldoc.core.document;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import com.ibm.icu.util.StringTokenizer;

/**
 * Comparators to sort documents with different options
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.1
 */
public abstract class DocumentComparator implements Comparator<AbstractDocument> {

	public static DocumentComparator ID_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return Long.valueOf(d1.getId()).compareTo(d2.getId());
		}
	};

	public static DocumentComparator FILENAME_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getFileName().compareTo(d2.getFileName());
		}
	};

	public static DocumentComparator SIZE_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return Long.valueOf(d1.getFileSize()).compareTo(d2.getFileSize());
		}
	};

	public static DocumentComparator VERSION_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getVersion().compareTo(d2.getVersion());
		}
	};

	public static DocumentComparator FILEVERSION_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getFileVersion().compareTo(d2.getFileVersion());
		}
	};

	public static DocumentComparator LASTMODIFIED_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getLastModified().compareTo(d2.getLastModified());
		}
	};

	public static DocumentComparator PUBLISHED_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getDate().compareTo(d2.getDate());
		}
	};

	public static DocumentComparator CREATED_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getCreation().compareTo(d2.getCreation());
		}
	};

	public static DocumentComparator CUSTOMID_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getCustomId().compareTo(d2.getCustomId());
		}
	};

	public static DocumentComparator TYPE_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getType().compareTo(d2.getType());
		}
	};

	public static DocumentComparator COMMENT_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			String val1 = d1.getComment();
			String val2 = d2.getComment();
			if (val1 == null && val2 == null)
				return 0;
			else if (val1 == null && val2 != null)
				return -1;
			else if (val1 != null && val2 == null)
				return 1;
			else
				return val1.compareTo(val2);
		}
	};

	public static DocumentComparator STARTPUB_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			Date val1 = d1.getStartPublishing();
			Date val2 = d2.getStartPublishing();
			if (val1 == null && val2 == null)
				return 0;
			else if (val1 == null && val2 != null)
				return -1;
			else if (val1 != null && val2 == null)
				return 1;
			else
				return val1.compareTo(val2);
		}
	};

	public static DocumentComparator STOPPUB_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			Date val1 = d1.getStopPublishing();
			Date val2 = d2.getStopPublishing();
			if (val1 == null && val2 == null)
				return 0;
			else if (val1 == null && val2 != null)
				return -1;
			else if (val1 != null && val2 == null)
				return 1;
			else
				return val1.compareTo(val2);
		}
	};

	public static DocumentComparator WFSTATUS_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			String val1 = d1.getWorkflowStatus();
			String val2 = d2.getWorkflowStatus();
			if (val1 == null && val2 == null)
				return 0;
			else if (val1 == null && val2 != null)
				return -1;
			else if (val1 != null && val2 == null)
				return 1;
			else
				return val1.compareTo(val2);
		}
	};

	public static DocumentComparator PUBSTATUS_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return Integer.valueOf(d1.getPublished()).compareTo(d2.getPublished());
		}
	};

	public static DocumentComparator descending(final Comparator<AbstractDocument> other) {
		return new DocumentComparator() {
			public int compare(AbstractDocument d1, AbstractDocument d2) {
				return -1 * other.compare(d1, d2);
			}
		};
	}

	public static DocumentComparator newComparatorForExtendedAttribute(final String attribute) {
		return new DocumentComparator() {
			@SuppressWarnings({ "rawtypes", "unchecked" })
			public int compare(AbstractDocument d1, AbstractDocument d2) {
				Comparable val1 = (Comparable) d1.getValue(attribute);
				Comparable val2 = (Comparable) d2.getValue(attribute);
				if (val1 == null && val2 == null)
					return 0;
				else if (val1 == null && val2 != null)
					return -1;
				else if (val1 != null && val2 == null)
					return 1;
				else {
					return val1.compareTo(val2);
				}
			}
		};
	}

	public static Comparator<AbstractDocument> getComparator(String sort) {
		StringTokenizer st = new StringTokenizer(sort, ",", false);
		List<DocumentComparator> comparators = new ArrayList<DocumentComparator>();

		while (st.hasMoreTokens()) {
			String token = st.nextToken().trim();
			String field = token.substring(0, token.indexOf(' '));
			boolean asc = "asc".equals(token.substring(token.indexOf(' ') + 1));

			DocumentComparator comp = null;
			if ("filename".equals(field))
				comp = FILENAME_SORT;
			else if ("id".equals(field))
				comp = ID_SORT;
			else if ("size".equals(field))
				comp = SIZE_SORT;
			else if ("version".equals(field))
				comp = VERSION_SORT;
			else if ("fileVersion".equals(field))
				comp = FILEVERSION_SORT;
			else if ("lastModified".equals(field))
				comp = LASTMODIFIED_SORT;
			else if ("published".equals(field))
				comp = PUBLISHED_SORT;
			else if ("created".equals(field))
				comp = CREATED_SORT;
			else if ("created".equals(field))
				comp = CUSTOMID_SORT;
			else if ("type".equals(field))
				comp = TYPE_SORT;
			else if ("comment".equals(field))
				comp = COMMENT_SORT;
			else if ("workflowStatus".equals(field))
				comp = WFSTATUS_SORT;
			else if ("startPublishing".equals(field))
				comp = STARTPUB_SORT;
			else if ("stopPublishing".equals(field))
				comp = STOPPUB_SORT;
			else if ("publishedStatus".equals(field))
				comp = PUBSTATUS_SORT;
			else if (field.startsWith("ext_"))
				comp = newComparatorForExtendedAttribute(field.substring(field.indexOf('_') + 1));

			if (comp != null && asc)
				comparators.add(comp);
			else if (comp != null && !asc)
				comparators.add(descending(comp));
		}

		return getComparator(comparators);
	}

	public static Comparator<AbstractDocument> getComparator(List<DocumentComparator> multipleOptions) {
		return new Comparator<AbstractDocument>() {
			public int compare(AbstractDocument d1, AbstractDocument d2) {
				for (DocumentComparator option : multipleOptions) {
					int result = option.compare(d1, d2);
					if (result != 0) {
						return result;
					}
				}
				return 0;
			}
		};
	}

	public static Comparator<AbstractDocument> getComparator(final DocumentComparator... multipleOptions) {
		return new Comparator<AbstractDocument>() {
			public int compare(AbstractDocument d1, AbstractDocument d2) {
				for (DocumentComparator option : multipleOptions) {
					int result = option.compare(d1, d2);
					if (result != 0) {
						return result;
					}
				}
				return 0;
			}
		};
	}
}
