package com.logicaldoc.core.folder;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

/**
 * Comparators to sort folders with different options
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.3
 */
public abstract class FolderComparator implements Comparator<Folder> {

	private FolderComparator() {
	}

	private static final FolderComparator ID_SORT = new FolderComparator() {
		@Override
		public int compare(Folder f1, Folder f2) {
			return Long.compare(f1.getId(), f2.getId());
		}
	};

	private static final FolderComparator NAME_SORT_CS = new FolderComparator() {
		@Override
		public int compare(Folder f1, Folder f2) {
			return f1.getName().compareTo(f2.getName());
		}
	};

	private static final FolderComparator NAME_SORT_CI = new FolderComparator() {
		@Override
		public int compare(Folder f1, Folder f2) {
			return f1.getName().toLowerCase().compareTo(f2.getName().toLowerCase());
		}
	};

	private static final FolderComparator LASTMODIFIED_SORT = new FolderComparator() {
		@Override
		public int compare(Folder f1, Folder f2) {
			return f1.getLastModified().compareTo(f2.getLastModified());
		}
	};

	private static final FolderComparator CREATED_SORT = new FolderComparator() {
		@Override
		public int compare(Folder f1, Folder f2) {
			return f1.getCreation().compareTo(f2.getCreation());
		}
	};

	private static final FolderComparator TEMPLATE_NAME_SORT_CS = new FolderComparator() {
		@Override
		public int compare(Folder f1, Folder f2) {
			if (f1.getTemplate() != null && f2.getTemplate() != null)
				return f1.getTemplate().getName().compareTo(f2.getTemplate().getName());
			else
				return f1.getTemplateName().compareTo(f2.getTemplateName());
		}
	};

	private static final FolderComparator TEMPLATE_NAME_SORT_CI = new FolderComparator() {
		@Override
		public int compare(Folder f1, Folder f2) {
			if (f1.getTemplate() != null && f2.getTemplate() != null)
				return f1.getTemplate().getName().compareTo(f2.getTemplate().getName());
			else
				return f1.getTemplateName().toLowerCase().compareTo(f2.getTemplateName().toLowerCase());
		}
	};

	private static final FolderComparator descending(final Comparator<Folder> other) {
		return new FolderComparator() {
			public int compare(Folder f1, Folder f2) {
				return -1 * other.compare(f1, f2);
			}
		};
	}

	private static final FolderComparator newComparatorForExtendedAttribute(final String attribute,
			boolean caseSensitive) {
		return new FolderComparator() {
			@SuppressWarnings({ "rawtypes", "unchecked" })
			public int compare(Folder f1, Folder f2) {
				Comparable val1 = (Comparable) f1.getValue(attribute);
				Comparable val2 = (Comparable) f2.getValue(attribute);

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
	private static final Map<String, FolderComparator> legacyComparators = new HashMap<>();

	static {
		legacyComparators.put("name-CS", NAME_SORT_CS);
		legacyComparators.put("name-CI", NAME_SORT_CI);

		legacyComparators.put("id-CS", ID_SORT);
		legacyComparators.put("id-CI", ID_SORT);

		legacyComparators.put("lastModified-CS", LASTMODIFIED_SORT);
		legacyComparators.put("lastModified-CI", LASTMODIFIED_SORT);

		legacyComparators.put("creation-CS", CREATED_SORT);
		legacyComparators.put("creation-CI", CREATED_SORT);

		legacyComparators.put("template-CS", TEMPLATE_NAME_SORT_CS);
		legacyComparators.put("template-CI", TEMPLATE_NAME_SORT_CI);
	}

	public static Comparator<Folder> getComparator(String sort) {
		StringTokenizer st = new StringTokenizer(sort, ",", false);
		List<Comparator<Folder>> comparators = new ArrayList<>();

		while (st.hasMoreTokens()) {
			String token = st.nextToken().trim();

			String field = token.substring(0, token.indexOf(' '));
			boolean asc = "asc".equals(token.substring(token.indexOf(' ') + 1));

			boolean caseSensitive = true;
			if (field.startsWith("lower(") || field.startsWith("upper(")) {
				caseSensitive = false;
				field = field.substring(field.indexOf('(') + 1, field.lastIndexOf(')'));
			}

			FolderComparator comp = null;
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

	private static Comparator<Folder> getComparator(List<Comparator<Folder>> multipleOptions) {
		return (f1, f2) -> {
			for (Comparator<Folder> option : multipleOptions) {
				int result = option.compare(f1, f2);
				if (result != 0) {
					return result;
				}
			}
			return 0;
		};
	}
}