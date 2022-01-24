package ru.itmo.wp.service;

import org.springframework.stereotype.Service;
import ru.itmo.wp.domain.Tag;
import ru.itmo.wp.repository.TagRepository;

@Service
public class TagService {
    private final TagRepository tagRepository;

    public TagService(TagRepository tagRepository) {
        this.tagRepository = tagRepository;
    }

    public Tag save(Tag tag) {
        Tag existingTag = tagRepository.findByName(tag.getName());
        if (existingTag == null) {
            tagRepository.save(tag);
            return tag;
        } else {
            return existingTag;
        }
    }
}
