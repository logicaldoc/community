package com.logicaldoc.core.document;

import java.util.HashSet;
import java.util.Set;

import org.hibernate.LazyInitializationException;

import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.SecurableObject;

/**
 * Basic concrete implementation of <code>AbstractDocument</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 1.0
 */
public class Document extends AbstractDocument implements SecurableObject {
	private static final long serialVersionUID = 1L;

	private Set<AccessControlEntry> acl = new HashSet<>();

	public Document() {
	}

	public Document(Document source) {
		copyAttributes(source);
		setId(source.getId());
		setOcrd(source.getOcrd());
		setOcrTemplateId(source.getOcrTemplateId());
		setBarcoded(source.getBarcoded());
		setBarcodeTemplateId(source.getBarcodeTemplateId());
		setTemplate(source.getTemplate());
		setTemplateId(source.getTemplateId());
		setTemplateName(source.getTemplateName());

		if (source.getIndexed() != INDEX_INDEXED)
			setIndexed(source.getIndexed());
		setCustomId(null);

		try {
			for (AccessControlEntry ace : source.getAccessControlList())
				acl.add(new AccessControlEntry(ace));
		} catch (LazyInitializationException x) {
			// may happen do nothing
		}
	}

	@Override
	public Set<AccessControlEntry> getAccessControlList() {
		return acl;
	}

	@Override
	public void setAccessControlList(Set<AccessControlEntry> acl) {
		this.acl = acl;
	}

	@Override
	public AccessControlEntry getAccessControlEntry(long groupId) {
		return acl.stream().filter(ace -> ace.getGroupId() == groupId).findFirst().orElse(null);
	}

	@Override
	public void addAccessControlEntry(AccessControlEntry ace) {
		if (!acl.add(ace)) {
			acl.remove(ace);
			acl.add(ace);
		}
	}
}