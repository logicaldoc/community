package com.logicaldoc.core.document;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

/**
 * Comparators to sort documents with different options
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.1
 */
public abstract class DocumentComparator implements Comparator<AbstractDocument> {

	private DocumentComparator() {
	}

	private static final DocumentComparator ID_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return Long.compare(d1.getId(), d2.getId());
		}
	};

	private static final DocumentComparator FILENAME_SORT_CS = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getFileName().compareTo(d2.getFileName());
		}
	};

	private static final DocumentComparator FILENAME_SORT_CI = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getFileName().toLowerCase().compareTo(d2.getFileName().toLowerCase());
		}
	};

	private static final DocumentComparator FILESIZE_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return Long.compare(d1.getFileSize(), d2.getFileSize());
		}
	};

	private static final DocumentComparator VERSION_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getVersion().compareTo(d2.getVersion());
		}
	};

	private static final DocumentComparator FILEVERSION_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getFileVersion().compareTo(d2.getFileVersion());
		}
	};

	private static final DocumentComparator LASTMODIFIED_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getLastModified().compareTo(d2.getLastModified());
		}
	};

	private static final DocumentComparator PUBLISHED_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getDate().compareTo(d2.getDate());
		}
	};

	private static final DocumentComparator CREATED_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getCreation().compareTo(d2.getCreation());
		}
	};

	private static final DocumentComparator CUSTOMID_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getCustomId().compareTo(d2.getCustomId());
		}
	};

	private static final DocumentComparator TYPE_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return d1.getType().compareTo(d2.getType());
		}
	};

	private static final DocumentComparator COMMENT_SORT_CS = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			String val1 = d1.getComment();
			String val2 = d2.getComment();
			if (val1 == null && val2 == null)
				return 0;
			else if (val1 == null)
				return -1;
			else if (val2 == null)
				return 1;
			else
				return val1.compareTo(val2);
		}
	};

	private static final DocumentComparator COMMENT_SORT_CI = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			String val1 = d1.getComment();
			String val2 = d2.getComment();
			if (val1 == null && val2 == null)
				return 0;
			else if (val1 == null)
				return -1;
			else if (val2 == null)
				return 1;
			else
				return val1.toLowerCase().compareTo(val2.toLowerCase());
		}
	};

	private static final DocumentComparator STARTPUB_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			Date val1 = d1.getStartPublishing();
			Date val2 = d2.getStartPublishing();
			if (val1 == null && val2 == null)
				return 0;
			else if (val1 == null)
				return -1;
			else if (val2 == null)
				return 1;
			else
				return val1.compareTo(val2);
		}
	};

	private static final DocumentComparator STOPPUB_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			Date val1 = d1.getStopPublishing();
			Date val2 = d2.getStopPublishing();
			if (val1 == null && val2 == null)
				return 0;
			else if (val1 == null)
				return -1;
			else if (val2 == null)
				return 1;
			else
				return val1.compareTo(val2);
		}
	};

	private static final DocumentComparator WFSTATUS_SORT_CS = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			String val1 = d1.getWorkflowStatus();
			String val2 = d2.getWorkflowStatus();
			if (val1 == null && val2 == null)
				return 0;
			else if (val1 == null)
				return -1;
			else if (val2 == null)
				return 1;
			else
				return val1.compareTo(val2);
		}
	};

	private static final DocumentComparator WFSTATUS_SORT_CI = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			String val1 = d1.getWorkflowStatus();
			String val2 = d2.getWorkflowStatus();
			if (val1 == null && val2 == null)
				return 0;
			else if (val1 == null)
				return -1;
			else if (val2 == null)
				return 1;
			else
				return val1.toLowerCase().compareTo(val2.toLowerCase());
		}
	};

	private static final DocumentComparator PUBSTATUS_SORT = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			return Integer.compare(d1.getPublished(), d2.getPublished());
		}
	};

	private static final DocumentComparator TEMPLATE_NAME_SORT_CS = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			if (d1.getTemplate() != null && d2.getTemplate() != null)
				return d1.getTemplate().getName().compareTo(d2.getTemplate().getName());
			else
				return d1.getTemplateName().compareTo(d2.getTemplateName());
		}
	};

	private static final DocumentComparator TEMPLATE_NAME_SORT_CI = new DocumentComparator() {
		@Override
		public int compare(AbstractDocument d1, AbstractDocument d2) {
			if (d1.getTemplate() != null && d2.getTemplate() != null)
				return d1.getTemplate().getName().compareTo(d2.getTemplate().getName());
			else
				return d1.getTemplateName().toLowerCase().compareTo(d2.getTemplateName().toLowerCase());
		}
	};

	private static final DocumentComparator descending(final Comparator<AbstractDocument> other) {
		return new DocumentComparator() {
			public int compare(AbstractDocument d1, AbstractDocument d2) {
				return -1 * other.compare(d1, d2);
			}
		};
	}

	private static final DocumentComparator newComparatorForExtendedAttribute(final String attribute,
			boolean caseSensitive) {
		return new DocumentComparator() {
			@SuppressWarnings({ "rawtypes", "unchecked" })
			public int compare(AbstractDocument d1, AbstractDocument d2) {
				Comparable val1 = (Comparable) d1.getValue(attribute);
				Comparable val2 = (Comparable) d2.getValue(attribute);
				if (val1 == null && val2 == null)
					return 0;
				else if (val1 == null)
					return -1;
				else if (val2 == null)
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

	/**
	 * Map of comparators for legacy fields, Key is fieldname-CS or
	 * fieldName-CI, value is the comparator
	 */
	private static final Map<String, DocumentComparator> legacyComparators = new HashMap<>();

	static {
		legacyComparators.put("fileName-CS", FILENAME_SORT_CS);
		legacyComparators.put("fileName-CI", FILENAME_SORT_CI);

		legacyComparators.put("id-CS", ID_SORT);
		legacyComparators.put("id-CI", ID_SORT);

		legacyComparators.put("fileSize-CS", FILESIZE_SORT);
		legacyComparators.put("fileSize-CI", FILESIZE_SORT);

		legacyComparators.put("size-CS", FILESIZE_SORT);
		legacyComparators.put("size-CI", FILESIZE_SORT);

		legacyComparators.put("version-CS", VERSION_SORT);
		legacyComparators.put("version-CI", VERSION_SORT);

		legacyComparators.put("fileVersion-CS", FILEVERSION_SORT);
		legacyComparators.put("fileVersion-CI", FILEVERSION_SORT);

		legacyComparators.put("lastModified-CS", LASTMODIFIED_SORT);
		legacyComparators.put("lastModified-CI", LASTMODIFIED_SORT);

		legacyComparators.put("date-CS", PUBLISHED_SORT);
		legacyComparators.put("date-CI", PUBLISHED_SORT);

		legacyComparators.put("creation-CS", CREATED_SORT);
		legacyComparators.put("creation-CI", CREATED_SORT);

		legacyComparators.put("customId-CS", CUSTOMID_SORT);
		legacyComparators.put("customId-CI", CUSTOMID_SORT);

		legacyComparators.put("type-CS", TYPE_SORT);
		legacyComparators.put("type-CI", TYPE_SORT);

		legacyComparators.put("comment-CS", COMMENT_SORT_CS);
		legacyComparators.put("comment-CI", COMMENT_SORT_CI);

		legacyComparators.put("workflowStatus-CS", WFSTATUS_SORT_CS);
		legacyComparators.put("workflowStatus-CI", WFSTATUS_SORT_CI);

		legacyComparators.put("startPublishing-CS", STARTPUB_SORT);
		legacyComparators.put("startPublishing-CI", STARTPUB_SORT);

		legacyComparators.put("stopPublishing-CS", STOPPUB_SORT);
		legacyComparators.put("stopPublishing-CI", STOPPUB_SORT);

		legacyComparators.put("publishedStatus-CS", PUBSTATUS_SORT);
		legacyComparators.put("publishedStatus-CI", PUBSTATUS_SORT);

		legacyComparators.put("template-CS", TEMPLATE_NAME_SORT_CS);
		legacyComparators.put("template-CI", TEMPLATE_NAME_SORT_CI);
	}

	public static Comparator<AbstractDocument> getComparator(String sort) {
		StringTokenizer st = new StringTokenizer(sort, ",", false);
		List<DocumentComparator> comparators = new ArrayList<>();

		while (st.hasMoreTokens()) {
			String token = st.nextToken().trim();

			String field = token.substring(0, token.indexOf(' '));
			boolean asc = "asc".equals(token.substring(token.indexOf(' ') + 1));

			boolean caseSensitive = true;
			if (field.startsWith("lower(") || field.startsWith("upper(")) {
				caseSensitive = false;
				field = field.substring(field.indexOf('(') + 1, field.lastIndexOf(')'));
			}

			DocumentComparator comp = null;
			if (field.startsWith("ext_"))
				comp = newComparatorForExtendedAttribute(field.substring(field.indexOf('_') + 1), caseSensitive);
			else
				comp = legacyComparators.get(field + "-" + (caseSensitive ? "CS" : "CI"));

			if (comp != null && asc)
				comparators.add(comp);
			else if (comp != null && !asc)
				comparators.add(descending(comp));
		}

		return getComparator(comparators);
	}

	public static Comparator<AbstractDocument> getComparator(List<DocumentComparator> multipleOptions) {
		return (d1, d2) -> {
			for (DocumentComparator option : multipleOptions) {
				int result = option.compare(d1, d2);
				if (result != 0) {
					return result;
				}
			}
			return 0;
		};
	}

	public static Comparator<AbstractDocument> getComparator(final DocumentComparator... multipleOptions) {
		return (d1, d2) -> {
			for (DocumentComparator option : multipleOptions) {
				int result = option.compare(d1, d2);
				if (result != 0) {
					return result;
				}
			}
			return 0;
		};
	}
}