#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/serialization/string.hpp>

#include "fileIndex.h"

namespace boost{
namespace serialization {
    
    template<class Archive>
    void serialize(Archive & ar, suffix_entry & entry, const unsigned int version) {
        ar & entry.file_id;
        ar & entry.suffix_idx;
    }

    template<class Archive>
    void serialize(Archive & ar, file & f, const unsigned int version) {
        ar & f.path;
        ar & f.name;
        ar & f.id;
    }

    template<class Archive>
    void serialize(Archive & ar, fileIndex & d, const unsigned int version) {
        ar & d.files;
        ar & d.suffixes;
    }
}
}

