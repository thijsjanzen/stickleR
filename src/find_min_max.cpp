
//  Created by Thijs Janzen on 11/04/2019.
//

#include <Rcpp.h>
#include <vector>

double find_site(const double& antenna,
                 const Rcpp::NumericMatrix& site_map) {

  double out = NA_REAL;

  for (int i = 0; i < site_map.nrow(); ++i) {
    if (site_map(i, 0) == antenna) {
      out = site_map(i, 1);
      break;
    }
  }
  return out;
}

//' function to get all locations
//' @param max_sec maxsec
//' @param times tt
//' @param antennae aa
//' @param site_map sm
//' @return vector of sites
//' @export
// [[Rcpp::export]]
std::vector<double> get_all_locations_cpp(double max_sec,
                                      const std::vector<double>& times,
                                      const std::vector<double>& antennae,
                                      const Rcpp::NumericMatrix& site_map) {

  std::vector<double> sites(antennae.size(), 0);
  for (size_t i = 0; i < antennae.size(); ++i) {
    sites[i] = find_site(antennae[i], site_map);
  }


  std::vector<double> out(max_sec, 0);
  // starting bit, extrapolating from first read
  for (int t = 0; t < times.front(); ++t) {
    out[t] = sites.front();
  }

  // in between:
  for (size_t i = 0; i < times.size() - 1; ++i) {
    auto start = times[i];
    auto end = times[i + 1];
    for (size_t j = start; j < end; ++j) {
      out[j] = sites[i];
    }
  }

  // end read:
  for (int t = times.back(); t < max_sec; ++t) {
    out[t] = sites.back();
  }

  return out;
}




