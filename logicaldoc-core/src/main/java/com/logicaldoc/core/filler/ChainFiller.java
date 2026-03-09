package com.logicaldoc.core.filler;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections.MapUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.history.History;
import com.logicaldoc.core.runtime.Aspect;
import com.logicaldoc.core.runtime.FeatureDisabledException;
import com.logicaldoc.core.runtime.RunLevel;
import com.logicaldoc.core.searchengine.SearchException;

import jakarta.persistence.Cacheable;
import jakarta.persistence.CascadeType;
import jakarta.persistence.DiscriminatorValue;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.JoinTable;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OrderColumn;

/**
 * A filler that fills using a list of other fillers
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.3
 */
@Entity
@DiscriminatorValue("chain")
@Cacheable
public class ChainFiller extends Filler {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(ChainFiller.class);

	@OneToMany(fetch = FetchType.EAGER, cascade = CascadeType.DETACH)
	@JoinTable(name = "ld_filler_chain", joinColumns = @JoinColumn(name = "ld_fillerid"), inverseJoinColumns = @JoinColumn(name = "ld_chainedid"))
	@OrderColumn(name = "ld_position")
	private List<Filler> chain = new ArrayList<>();

	public List<Filler> getChain() {
		return chain;
	}

	public void setChain(List<Filler> chain) {
		this.chain = chain;
	}

	@Override
	public void fill(Fillable fillable, String content, History transaction, Map<String, Object> dictionary)
			throws PersistenceException, IOException, FeatureDisabledException, SearchException {
		if (!RunLevel.current().aspectEnabled(Aspect.AUTOFILL))
			return;

		if (!transaction.isFill()) {
			if (log.isDebugEnabled())
				log.debug("Skiping fill of object {}", fillable);
			return;
		}

		Map<String, Object> pipelineDict = new HashMap<>();
		if (MapUtils.isNotEmpty(dictionary))
			pipelineDict.putAll(dictionary);

		for (Filler filler : chain) {
			log.debug("invoking filler {}", filler);
			filler.fill(fillable, content, transaction, pipelineDict);
		}
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((chain == null) ? 0 : chain.hashCode());
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
		ChainFiller other = (ChainFiller) obj;
		if (chain == null) {
			if (other.chain != null)
				return false;
		} else if (!chain.equals(other.chain))
			return false;
		return true;
	}
}