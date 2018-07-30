#include <example-blas/example-blas.h>
#include "test_prelude.h"

using namespace std;

TEST(print_infos, case0) {
    cout 
        << "Example-blas infos" << endl
        << endl
        << "The current version is " << example_blas_version_str() << endl
        << "\t Major version: " << example_blas_version_major() << endl
        << "\t Minor version: " << example_blas_version_minor() << endl
        << "\t Patch version: " << example_blas_version_patch() << endl
        << "\t Diff  version: " << example_blas_version_diff() << endl
        << endl
        << "The current accelerate backend is " << example_blas_backend_type_str() << endl
        << "Accelerate backend id is " << example_blas_backend_type_id() << endl;
}