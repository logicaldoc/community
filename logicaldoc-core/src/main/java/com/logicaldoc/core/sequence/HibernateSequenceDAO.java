package com.logicaldoc.core.sequence;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;

import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of <code>SequenceDAO</code>. <br>
 * Sequences are implemented ad Generics whose type is 'sequence' and subtype is
 * the sequence name.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
@Repository("sequenceDAO")
@Transactional
public class HibernateSequenceDAO extends HibernatePersistentObjectDAO<Sequence> implements SequenceDAO {

	private static final String TENANTID = "tenantId";

	private static final String AND = " and ";

	private HibernateSequenceDAO() {
		super(Sequence.class);
		super.log = LoggerFactory.getLogger(HibernateSequenceDAO.class);
	}

	@Override
	public synchronized void reset(String sequence, long objectId, long tenantId, long value)
			throws PersistenceException {
		synchronized (SequenceDAO.class) {
			Sequence seq = findByAlternateKey(sequence, objectId, tenantId);
			if (seq == null)
				seq = new Sequence();
			seq.setName(sequence);
			seq.setObjectId(objectId);
			seq.setTenantId(tenantId);
			seq.setLastReset(new Date());
			seq.setValue(value);
			store(seq);
		}
	}

	@Override
	public synchronized long next(String sequence, long objectId, long tenantId, long increment)
			throws PersistenceException {
		synchronized (SequenceDAO.class) {
			Sequence seq = findByAlternateKey(sequence, objectId, tenantId);
			if (seq == null) {
				reset(sequence, objectId, tenantId, increment);
				return increment;
			} else {
				jdbcUpdate(
						"update ld_sequence set ld_value = ld_value + %d, ld_recordversion = ld_recordversion + 1, ld_lastmodified = CURRENT_TIMESTAMP where ld_id = %d"
								.formatted(increment, seq.getId()));
				evict(seq.getId());
				return queryForLong("select ld_value from ld_sequence where ld_id = %d".formatted(seq.getId()));
			}
		}
	}

	@Override
	public synchronized long next(String sequence, long objectId, long tenantId) throws PersistenceException {
		return this.next(sequence, objectId, tenantId, 1L);
	}

	@Override
	public long getCurrentValue(String sequence, long objectId, long tenantId) throws PersistenceException {
		Sequence seq = findByAlternateKey(sequence, objectId, tenantId);
		if (seq == null)
			return 0L;
		else
			return seq.getValue();
	}

	@Override
	public List<Sequence> findByName(String name, long tenantId) {
		String query = " " + ENTITY + ".tenantId = :tenantId " + AND + ENTITY + ".name like :name ";

		try {
			return findByWhere(query, Map.of(TENANTID, tenantId, "name", name + "%"), null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public Sequence findByAlternateKey(String name, long objectId, long tenantId) throws PersistenceException {
		List<Sequence> sequences = findSequences(name, objectId, tenantId);

		Sequence sequence = null;

		// It's incredible but the findByWhere sometimes doesn't find the
		// sequence so finding by the ID is safer
		if (sequences.isEmpty()) {
			sequence = findSequence(name, objectId, tenantId, sequence);
		} else {
			sequence = sequences.get(0);
		}

		if (sequence == null)
			log.debug("Unable to find sequence {},{},{}", name, objectId, tenantId);
		else
			refresh(sequence);

		return sequence;
	}

	private Sequence findSequence(String sequenceName, long objectId, long tenantId, Sequence sequence)
			throws PersistenceException {
		String query = "select ld_id from ld_sequence where ld_name = :name and ld_objectid = :objectId and ld_tenantid = :tenantId";

		long sequenceId = queryForLong(query, Map.of(TENANTID, tenantId, "objectId", objectId, "name", sequenceName));
		if (sequenceId != 0L) {
			sequence = findById(sequenceId);
			refresh(sequence);
		}
		return sequence;
	}

	private List<Sequence> findSequences(String sequenceName, long objectId, long tenantId) throws PersistenceException {
		String query = " " + ENTITY + ".tenantId = :tenantId ";
		query += AND + ENTITY + ".objectId = :objectId ";
		query += AND + ENTITY + ".name = :name ";

		return findByWhere(query, Map.of(TENANTID, tenantId, "objectId", objectId, "name", sequenceName), null, null);
	}

	@Override
	public void delete(String name, long objectId, long tenantId) throws PersistenceException {
		Sequence seq = findByAlternateKey(name, objectId, tenantId);
		if (seq != null)
			delete(seq.getId());
	}

	@Override
	public void delete(long id, int code) throws PersistenceException {
		Sequence seq = findById(id);
		if (seq != null) {
			seq.setName(seq.getId() + "." + seq.getName());
			seq.setDeleted(code);
			store(seq);
		}
	}
}