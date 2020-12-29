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

	private static DocumentComparator ID_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return Long.valueOf(d1.getId()).compareTo(d2.getId());
		}
	};

	private static DocumentComparator FILENAME_SORT_CS = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getFileName().compareTo(d2.getFileName());
		}
	};

	private static DocumentComparator FILENAME_SORT_CI = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getFileName().toLowerCase().compareTo(d2.getFileName().toLowerCase());
		}
	};

	private static DocumentComparator FILESIZE_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return Long.valueOf(d1.getFileSize()).compareTo(d2.getFileSize());
		}
	};

	private static DocumentComparator VERSION_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getVersion().compareTo(d2.getVersion());
		}
	};

	private static DocumentComparator FILEVERSION_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getFileVersion().compareTo(d2.getFileVersion());
		}
	};

	private static DocumentComparator LASTMODIFIED_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getLastModified().compareTo(d2.getLastModified());
		}
	};

	private static DocumentComparator PUBLISHED_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getDate().compareTo(d2.getDate());
		}
	};

	private static DocumentComparator CREATED_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getCreation().compareTo(d2.getCreation());
		}
	};

	private static DocumentComparator CUSTOMID_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getCustomId().compareTo(d2.getCustomId());
		}
	};

	private static DocumentComparator TYPE_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getType().compareTo(d2.getType());
		}
	};

	private static DocumentComparator COMMENT_SORT_CS = new DocumentComparator() {
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

	private static DocumentComparator COMMENT_SORT_CI = new DocumentComparator() {
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
				return val1.toLowerCase().compareTo(val2.toLowerCase());
		}
	};

	private static DocumentComparator STARTPUB_SORT = new DocumentComparator() {
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

	private static DocumentComparator STOPPUB_SORT = new DocumentComparator() {
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

	private static DocumentComparator WFSTATUS_SORT_CS = new DocumentComparator() {
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

	private static DocumentComparator WFSTATUS_SORT_CI = new DocumentComparator() {
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
				return val1.toLowerCase().compareTo(val2.toLowerCase());
		}
	};

	private static DocumentComparator PUBSTATUS_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return Integer.valueOf(d1.getPublished()).compareTo(d2.getPublished());
		}
	};

	private static DocumentComparator TEMPLATE_NAME_SORT_CS = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			if (d1.getTemplate() != null && d2.getTemplate() != null)
				return d1.getTemplate().getName().compareTo(d2.getTemplate().getName());
			else
				return d1.getTemplateName().compareTo(d2.getTemplateName());
		}
	};

	private static DocumentComparator TEMPLATE_NAME_SORT_CI = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			if (d1.getTemplate() != null && d2.getTemplate() != null)
				return d1.getTemplate().getName().compareTo(d2.getTemplate().getName());
			else
				return d1.getTemplateName().toLowerCase().compareTo(d2.getTemplateName().toLowerCase());
		}
	};

	private static DocumentComparator descending(final Comparator<AbstractDocument> other) {
		return new DocumentComparator() {
			public int compare(AbstractDocument d1, AbstractDocument d2) {
				return -1 * other.compare(d1, d2);
			}
		};
	}

	private static DocumentComparator newComparatorForExtendedAttribute(final String attribute, boolean caseSensitive) {
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
					if (val1 instanceof String && !caseSensitive)
						return val1.toString().toLowerCase().compareTo(val2.toString().toLowerCase());
					else
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

			boolean caseSensitive = true;
			if (field.startsWith("lower(") || field.startsWith("upper(")) {
				caseSensitive = false;
				field = field.substring(field.indexOf('(')+1, field.lastIndexOf(')'));
			}

			DocumentComparator comp = null;
			if ("filename".equalsIgnoreCase(field))
				comp = caseSensitive ? FILENAME_SORT_CS : FILENAME_SORT_CI;
			else if ("id".equalsIgnoreCase(field))
				comp = ID_SORT;
			else if ("fileSize".equalsIgnoreCase(field) || "size".equalsIgnoreCase(field))
				comp = FILESIZE_SORT;
			else if ("version".equalsIgnoreCase(field))
				comp = VERSION_SORT;
			else if ("fileVersion".equalsIgnoreCase(field))
				comp = FILEVERSION_SORT;
			else if ("lastModified".equalsIgnoreCase(field))
				comp = LASTMODIFIED_SORT;
			else if ("published".equalsIgnoreCase(field) || "date".equalsIgnoreCase(field))
				comp = PUBLISHED_SORT;
			else if ("created".equalsIgnoreCase(field))
				comp = CREATED_SORT;
			else if ("created".equalsIgnoreCase(field))
				comp = CUSTOMID_SORT;
			else if ("type".equalsIgnoreCase(field))
				comp = TYPE_SORT;
			else if ("comment".equalsIgnoreCase(field))
				comp = caseSensitive ? COMMENT_SORT_CS : COMMENT_SORT_CI;
			else if ("workflowStatus".equalsIgnoreCase(field))
				comp = caseSensitive ? WFSTATUS_SORT_CS : WFSTATUS_SORT_CI;
			else if ("startPublishing".equalsIgnoreCase(field))
				comp = STARTPUB_SORT;
			else if ("stopPublishing".equalsIgnoreCase(field))
				comp = STOPPUB_SORT;
			else if ("publishedStatus".equalsIgnoreCase(field))
				comp = PUBSTATUS_SORT;
			if ("template".equalsIgnoreCase(field))
				comp = caseSensitive ? TEMPLATE_NAME_SORT_CS : TEMPLATE_NAME_SORT_CI;
			else if (field.startsWith("ext_"))
				comp = newComparatorForExtendedAttribute(field.substring(field.indexOf('_') + 1), caseSensitive);

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
