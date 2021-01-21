### Answer body id and names data

#' Return the ids and names of Parliamentary question answer bodies
#'
#' \code{get_answer_bodies} returns the ids and names of bodies responsible
#' for answer Parliamentary questions.
#'
#'  id      -- The unique id of an answer body
#'  body    -- The name of an answer body
#'
#' @return A tibble with answer body ids and names.
#' @export

get_answer_bodies <- function() {

    answer_bodies <- "
        id,     name
        88,     Attorney General
        53,     Cabinet Office
        200,    Chairman of Committees (HL)
        9,      Church Commissioners
        201,    Department for Business Energy and Industrial Strategy
        10,     Department for Digital Culture Media and Sport
        60,     Department for Education
        13,     Department for Environment Food and Rural Affairs
        202,    Department for International Trade
        27,     Department for Transport
        29,     Department for Work and Pensions
        17,     Department of Health and Social Care
        208,    Foreign Commonwealth and Development Office
        1,      Home Office
        18,     House of Commons Commission
        91,     House of Commons Members Estimate Committee
        34,     Leader of the House of Commons
        92,     Leader of the House of Lords
        206,    Lord Speaker
        31,     Minister for Women and Equalities
        57,     Minister without Portfolio
        11,     Ministry of Defence
        7,      Ministry of Housing Communities and Local Government
        54,     Ministry of Justice
        21,     Northern Ireland Office
        207,    Parliamentary Works Sponsor Body
        23,     Prime Minister
        24,     Public Accounts Commission
        2,      Scotland Office
        204,    Senior Deputy Speaker (HL)
        90,     Speaker's Committee for the Independent Parliamentary Standards Authority
        36,     Speaker's Committee on the Electoral Commission
        14,     Treasury
        28,     Wales Office"

    readr::read_csv(answer_bodies,
                   trim_ws = TRUE,
                   col_types = readr::cols(
                       id = readr::col_character(),
                       name = readr::col_character()))
}
