package com.logicaldoc.core.metadata;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import jakarta.persistence.MappedSuperclass;

import org.apache.commons.collections.CollectionUtils;

import com.logicaldoc.core.PersistentObject;

/**
 * An extensible object is able to store an indeterminate number of attributes.
 * Each attribute has a name and a string value.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
@MappedSuperclass
public abstract class ExtensibleObject extends PersistentObject implements Extensible {

	private static final long serialVersionUID = 1L;

	@Override
	public List<Attribute> getValueAttributes(String name) {
		Set<String> valueNames = getValueAttributesName(name);

		// The names are ordered
		List<Attribute> values = new ArrayList<>();
		for (String n : valueNames) {
			Attribute val = getAttributes().get(n);
			val.setName(n);
			values.add(val);
		}

		return values;
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
		if (getAttributes() != null && getAttributes().get(name) != null)
			return getAttributes().get(name);
		else
			return null;
	}

	@Override
	public List<String> getAttributeNames() {
		List<String> names = new ArrayList<>();
		if (getAttributes() != null)
			names = getAttributes().keySet().stream().toList();
		return names;
	}

	@Override
	public List<String> getAttributeNames(long setId) {
		List<String> names = new ArrayList<>();
		if (getAttributes() != null) {
			for (String name : getAttributes().keySet()) {
				Attribute att = getAttribute(name);
				if (att.getSetId() != null && setId == att.getSetId())
					names.add(name);
			}
		}
		return names;
	}

	@Override
	public void removeAttribute(String name) {
		if (getAttributes() != null && getAttributes().containsKey(name)) {
			Set<String> valueNames = getValueAttributesName(name);
			for (String n : valueNames)
				getAttributes().remove(n);
		}
	}

	@Override
	public Attribute getAttributeAtPosition(int position) {
		if (position < 0)
			return null;
		List<Attribute> attrs = new ArrayList<>(getAttributes().values());
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

	/**
	 * Puts a new attribute, if the attribute already exists his position and
	 * label are left untouched
	 * 
	 * @param name name of the attribute
	 * @param attribute the attribute instance
	 */
	@Override
	public void setAttribute(String name, Attribute attribute) {
		Attribute newAttribute = new Attribute(attribute);
		Attribute oldAttribute = getAttributes().get(name);
		if (oldAttribute != null) {
			newAttribute.setPosition(oldAttribute.getPosition());
			newAttribute.setLabel(oldAttribute.getLabel());
		} else {
			newAttribute.setPosition(getLastPosition() + 1);
		}
		getAttributes().put(name, newAttribute);
	}

	@Override
	public Attribute setValue(String name, Object value) {
		Attribute ext = getAttribute(name);
		if (ext == null) {
			ext = new Attribute();
			ext.setPosition(getLastPosition() + 1);
		}
		ext.setValue(value);
		getAttributes().put(name, ext);
		return getAttributes().get(name);
	}

	protected int getLastPosition() {
		int position = 0;

		if (getAttributes() != null)
			for (Attribute att : getAttributes().values()) {
				if (position < att.getPosition())
					position = att.getPosition();
			}

		return position;
	}

	/**
	 * Retrieves the ordered set of the names of the attributes representing the
	 * different values
	 * 
	 * @param name name of the attribute to get values from
	 * 
	 * @return the set of names
	 */
	protected Set<String> getValueAttributesName(String name) {
		TreeSet<String> attNames = new TreeSet<>();
		for (String n : getAttributes().keySet()) {
			if (n.equals(name) || name.equals(getAttribute(n).getParent()))
				attNames.add(n);
		}
		return attNames;
	}
}