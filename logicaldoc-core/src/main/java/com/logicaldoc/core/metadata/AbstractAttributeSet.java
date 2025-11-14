package com.logicaldoc.core.metadata;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.collections.CollectionUtils;

import jakarta.persistence.Column;
import jakarta.persistence.MappedSuperclass;

/**
 * A base class for attribute sets and templates
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.8.3
 */
@MappedSuperclass
public abstract class AbstractAttributeSet extends ExtensibleObject {

	private static final long serialVersionUID = 1L;

	/**
	 * The AttributeSet's default type: 0
	 */
	public static final int TYPE_DEFAULT = 0;

	@Column(name = "ld_name", length = 255, nullable = false)
	private String name;

	@Column(name = "ld_label", length = 255)
	private String label;

	@Column(name = "ld_description", length = 2000)
	private String description;

	@Column(name = "ld_readonly", nullable = false)
	private boolean readonly = false;

	@Column(name = "ld_type", nullable = false)
	private int type = TYPE_DEFAULT;

	public abstract Map<String, Attribute> getTemplateAttributes();

	public abstract void setTemplateAttributes(Map<String, Attribute> templateAttributes);

	public Attribute getTemplateAttribute(String name) {
		return getTemplateAttributes().get(name);
	}

	@Override
	public Map<String, Attribute> getAttributes() {
		return new AttributeMapWrapper(getTemplateAttributes());
	}

	@Override
	public void setAttributes(Map<String, Attribute> attributes) {
		getTemplateAttributes().clear();
		for (Map.Entry<String, ? extends Attribute> att : attributes.entrySet()) {
			if (att.getValue() instanceof Attribute ta)
				getTemplateAttributes().put(att.getKey(), ta);
		}
	}

	@Override
	public List<Attribute> getValueAttributes(String name) {
		Set<String> valueNames = getValueAttributesName(name);

		// The names are ordered
		List<Attribute> values = new ArrayList<>();
		for (String n : valueNames) {
			Attribute val = getTemplateAttributes().get(n);
			val.setName(n);
			values.add(val);
		}

		return values;
	}

	@Override
	protected Set<String> getValueAttributesName(String name) {
		TreeSet<String> attNames = new TreeSet<>();
		for (String n : getTemplateAttributes().keySet()) {
			if (n.equals(name) || name.equals(getAttribute(n).getParent()))
				attNames.add(n);
		}
		return attNames;
	}

	@Override
	public List<Object> getValues(String name) {
		List<Attribute> attrs = getValueAttributes(name);
		return attrs.stream().map(a -> a.getValue()).toList();
	}

	@Override
	public Object getValue(String name) {
		Attribute att = getAttribute(name);
		if (att != null)
			return att.getValue();
		else
			return null;
	}

	@Override
	public Attribute getAttribute(String name) {
		if (getTemplateAttributes() != null && getTemplateAttributes().get(name) != null)
			return getTemplateAttributes().get(name);
		else
			return null;
	}

	@Override
	public List<String> getAttributeNames() {
		List<String> names = new ArrayList<>();
		if (getTemplateAttributes() != null)
			names = getTemplateAttributes().keySet().stream().toList();
		return names;
	}

	@Override
	public List<String> getAttributeNames(long setId) {
		List<String> names = new ArrayList<>();
		if (getTemplateAttributes() != null) {
			for (String attributeName : getTemplateAttributes().keySet()) {
				Attribute att = getAttribute(attributeName);
				if (att.getSetId() != null && setId == att.getSetId())
					names.add(attributeName);
			}
		}
		return names;
	}

	@Override
	public void removeAttribute(String name) {
		if (getTemplateAttributes() != null && getTemplateAttributes().containsKey(name)) {
			Set<String> valueNames = getValueAttributesName(name);
			for (String n : valueNames)
				getTemplateAttributes().remove(n);
		}
	}

	@Override
	public Attribute getAttributeAtPosition(int position) {
		if (position < 0)
			return null;
		List<Attribute> attrs = new ArrayList<>(getTemplateAttributes().values());
		if (position >= attrs.size())
			return null;
		Attribute attribute = null;
		for (Attribute extendedAttribute : attrs) {
			if (extendedAttribute.getPosition() == position) {
				attribute = extendedAttribute;
				break;
			}
		}
		return attribute;
	}

	@Override
	public List<Attribute> setValues(String name, List<Object> values) {
		// clean the attributes that store the actual multiple values
		Set<String> valNames = getValueAttributesName(name);
		for (String n : valNames) {
			if (name.equals(n))
				continue;
			removeAttribute(n);
		}

		List<Attribute> attrs = new ArrayList<>();

		if (CollectionUtils.isEmpty(values))
			return attrs;

		Attribute master = setValue(name, values.get(0));
		attrs.add(master);

		if (values.size() > 1) {
			master.setMultiple(1);
			NumberFormat nf = new DecimalFormat("0000");
			for (int i = 1; i < values.size(); i++) {
				Attribute attribute = setValue(name + "-" + nf.format(i), values.get(i));
				attribute.setParent(name);
				attrs.add(attribute);
			}
		}

		for (Attribute attribute : attrs)
			attribute.setPosition(master.getPosition());

		return attrs;
	}

	@Override
	public void setAttribute(String name, Attribute attr) {
		if (attr instanceof Attribute attribute) {
			Attribute newAttribute = new Attribute(attribute);
			Attribute oldAttribute = getTemplateAttributes().get(name);
			if (oldAttribute != null) {
				newAttribute.setPosition(oldAttribute.getPosition());
				newAttribute.setLabel(oldAttribute.getLabel());
			} else {
				newAttribute.setPosition(getLastPosition() + 1);
			}
			getTemplateAttributes().put(name, newAttribute);
		}
	}

	@Override
	public Attribute setValue(String name, Object value) {
		Attribute ext = getAttribute(name);
		if (ext == null) {
			ext = new Attribute();
			ext.setPosition(getLastPosition() + 1);
		}
		ext.setValue(value);
		getTemplateAttributes().put(name, ext);
		return ext;
	}

	@Override
	protected int getLastPosition() {
		int position = 0;

		if (getTemplateAttributes() != null)
			for (Attribute att : getTemplateAttributes().values()) {
				if (position < att.getPosition())
					position = att.getPosition();
			}

		return position;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public boolean isReadonly() {
		return readonly;
	}

	public void setReadonly(boolean readonly) {
		this.readonly = readonly;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	@Override
	public Long getTemplateId() {
		return null;
	}

	@Override
	public void setTemplateId(Long templateId) {
		// Not implemented
	}

	@Override
	public String getTemplateName() {
		return null;
	}

	@Override
	public void setTemplateName(String templateName) {
		// Not implemented
	}

	@Override
	public Template getTemplate() {
		return null;
	}

	@Override
	public void setTemplate(Template template) {
		// not implemented
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((name == null) ? 0 : name.hashCode());
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
		AbstractAttributeSet other = (AbstractAttributeSet) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}

}